package info.kgeorgiy.ja.mochekov.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.tools.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Implementation of the {@link Impler} interface that generates implementations
 * for classes and interfaces.
 */
public class Implementor implements JarImpler {

    /**
     * Main method for CLI access.
     *
     * @param args command line arguments:
     * Optional {@code -jar} flag to create a JAR file
     * Class or interface name to implement
     * Output path for the implementation file or JAR file
     *
     */
    public static void main(String[] args) {
        if (args == null || (args.length != 2 && args.length != 3)) {
            System.err.println("Usage: java Implementor [-jar] <class> <output>");
            return;
        }

        try {
            Implementor implementor = new Implementor();
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            } else if ("-jar".equals(args[0])) {
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                System.err.println("Invalid arguments: " + String.join(" ", args));
            }
        } catch (ClassNotFoundException e) {
            System.err.println("Class not found: " + e.getMessage());
        } catch (ImplerException e) {
            System.err.println("Error during implementation: " + e.getMessage());
        }
    }

    /**
     * Produces code implementing class or interface specified by provided {@code token}.
     * <p>
     * Generated class' name should be the same as the class name of the type token with {@code Impl} suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * {@code root} directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to {@code $root/java/util/ListImpl.java}
     *
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     * generated.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        validateClass(token);
        String implName = token.getSimpleName() + "Impl";
        String packageName = token.getPackage() != null ? token.getPackage().getName() : "";

        StringBuilder code = new StringBuilder();
        writePackage(packageName, code);
        writeClassDeclaration(token, implName, code);
        if (!token.isInterface()) {
            writeConstructors(token, implName, code);
        }
        writeMethods(token, code);
        code.append("}");

        saveToFile(token, root, implName, code.toString());
    }

    /**
     * Produces <var>.jar</var> file implementing class or interface specified by provided <var>token</var>.
     * <p>
     * Generated class' name should be the same as the class name of the type token with <var>Impl</var> suffix
     * added.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        Path tempDir = null;
        try {
            tempDir = Paths.get(System.getProperty("java.io.tmpdir"), "jar-implementor");
            Files.createDirectories(tempDir); // LINUX SECURITY
            implement(token, tempDir);
            compileImplementation(token, tempDir);
            createJarFile(token, tempDir, jarFile);
        } catch (IOException e) {
            throw new ImplerException("Error creating JAR file: " + e.getMessage(), e);
        } finally {
            if (tempDir != null) {
                try {
                    deleteDirectory(tempDir);
                } catch (IOException e) {
                    System.err.println("Error deleting temporary directory: " + e.getMessage());
                }
            }
        }
    }

    /**
     * Validates that the specified class can be implemented.
     *
     * @param token class to validate.
     * @throws ImplerException if class cannot be implemented.
     */
    protected void validateClass(Class<?> token) throws ImplerException {
        if (token.isPrimitive() || token.isArray() || token == Enum.class || token == Record.class
                || Modifier.isFinal(token.getModifiers()) || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Cannot implement: " + token.getName() + ". Invalid type.");
        }
    }

    /**
     * Writes package declaration to the specified {@link StringBuilder}.
     *
     * @param packageName name of the package.
     * @param code the {@link StringBuilder} to append package declaration.
     */
    protected void writePackage(String packageName, StringBuilder code) {
        if (!packageName.isEmpty()) {
            code.append("package ").append(packageName).append(";")
                    .append(System.lineSeparator()).append(System.lineSeparator());
        }
    }

    /**
     * Writes class declaration to the specified {@link StringBuilder}.
     *
     * @param token type token for which implementation is being generated.
     * @param implName name of the implementation class.
     * @param code the {@link StringBuilder} to append class declaration.
     */
    protected void writeClassDeclaration(Class<?> token, String implName, StringBuilder code) {
        code.append("public class ").append(implName).append(" ");
        if (token.isInterface()) {
            code.append("implements ");
        } else {
            code.append("extends ");
        }
        code.append(token.getCanonicalName()).append(" {").append(System.lineSeparator());
    }

    /**
     * Writes constructors for the implementation class to the specified {@link StringBuilder}.
     *
     * @param token class for which implementation is being generated.
     * @param implName name of the implementation class.
     * @param code the {@link StringBuilder} to append constructors.
     * @throws ImplerException if class has no accessible constructors.
     */
    protected void writeConstructors(Class<?> token, String implName, StringBuilder code) throws ImplerException {
        List<Constructor<?>> constructors = Arrays.stream(token.getDeclaredConstructors())
                .filter(constructor -> !Modifier.isPrivate(constructor.getModifiers()))
                .toList();

        if (constructors.isEmpty()) {
            throw new ImplerException("No accessible constructors in " + token.getName());
        }

        for (Constructor<?> constructor : constructors) {
            writeConstructor(constructor, implName, code);
        }
    }

    /**
     * Writes a single constructor for the implementation class to the specified {@link StringBuilder}.
     *
     * @param constructor constructor to implement.
     * @param implName name of the implementation class.
     * @param code the {@link StringBuilder} to append constructor.
     */
    protected void writeConstructor(Constructor<?> constructor, String implName, StringBuilder code) {
        int mods = constructor.getModifiers() &
                ~Modifier.ABSTRACT & ~Modifier.PROTECTED & ~Modifier.TRANSIENT;
        code.append("    ").append(Modifier.toString(mods)).append(" ")
                .append(implName).append("(");

        Parameter[] parameters = constructor.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            if (i > 0) { code.append(", "); }
            Parameter param = parameters[i];
            String typeName = param.getType().getCanonicalName();
            code.append(typeName).append(" arg").append(i);
        }

        code.append(")");
        Class<?>[] exceptions = constructor.getExceptionTypes();
        if (exceptions.length > 0) {
            code.append(" throws ");
            for (int i = 0; i < exceptions.length; i++) {
                if (i > 0) { code.append(", "); }
                code.append(exceptions[i].getCanonicalName());
            }
        }

        code.append(" {").append(System.lineSeparator()).append("        super(");
        code.append(IntStream.range(0, parameters.length)
                .mapToObj(i -> "arg" + i)
                .collect(Collectors.joining(", ")));

        code.append(");").append(System.lineSeparator()).append("    }").append(System.lineSeparator());
    }

    /**
     * Writes method parameters to the specified {@link StringBuilder}.
     *
     * @param parameters array of method parameters.
     * @param code the {@link StringBuilder} to append parameters.
     */
    protected void writeParameters(Parameter[] parameters, StringBuilder code) {
        code.append(IntStream.range(0, parameters.length)
                .mapToObj(i -> parameters[i].getType().getCanonicalName() + " arg" + i)
                .collect(Collectors.joining(", ")));
    }

    /**
     * Writes method implementations for the class to the specified {@link StringBuilder}.
     *
     * @param token class for which method implementations are being generated.
     * @param code the {@link StringBuilder} to append method implementations.
     */
    protected void writeMethods(Class<?> token, StringBuilder code) {
        Map<MethodSignature, Method> methodsToImplement = getMethodsToImplement(token);
        for (Method method : methodsToImplement.values()) {
            writeMethod(method, code);
        }
    }

    /**
     * Finds all abstract methods that need to be implemented.
     *
     * @param token class or interface for which implementation is being generated.
     * @return a map from method signatures to abstract methods that need to be implemented.
     */
    protected Map<MethodSignature, Method> getMethodsToImplement(Class<?> token) {
        Map<MethodSignature, Method> methods = new HashMap<>();

        for (Method method : token.getMethods()) {
            if (Modifier.isAbstract(method.getModifiers())) {
                methods.put(new MethodSignature(method), method);
            }
        }

        Class<?> current = token;
        while (current != null) {
            for (Method method : current.getDeclaredMethods()) {
                if (Modifier.isAbstract(method.getModifiers()) &&
                        !Modifier.isPublic(method.getModifiers())) {
                    MethodSignature signature = new MethodSignature(method);
                    if (!methods.containsKey(signature) ||
                            methods.get(signature).getReturnType()
                                    .isAssignableFrom(method.getReturnType())) {
                        methods.put(signature, method);
                    }
                }
            }
            current = current.getSuperclass();
        }

        return methods;
    }

    /**
     * Writes a method implementation to the specified {@link StringBuilder}.
     *
     * @param method method to implement.
     * @param code the {@link StringBuilder} to append method implementation.
     */
    protected void writeMethod(Method method, StringBuilder code) {
        Class<?> returnTypeClass = method.getReturnType();

        code.append("    @Override").append(System.lineSeparator()).append("    public ")
                .append(returnTypeClass.getCanonicalName())
                    .append(" ").append(method.getName()).append("(");

        writeParameters(method.getParameters(), code);
        code.append(") {").append(System.lineSeparator());

        if (!method.getReturnType().equals(void.class)) {
            code.append("        return ").append(getDefaultValue(method.getReturnType()))
                    .append(";").append(System.lineSeparator());
        }

        code.append("    }").append(System.lineSeparator()).append(System.lineSeparator());
    }

    /**
     * Returns default value for the specified type.
     *
     * @param type type to get default value for.
     * @return string representation of the default value for the type.
     */
    protected String getDefaultValue(Class<?> type) {
        if (type == boolean.class) {
            return "false";
        } else if (type == void.class) {
            return "";
        } else if (type == float.class) {
            return "0.0f";
        } else if (type == double.class) {
            return "0.0d";
        } else if (type.isPrimitive()) {
            return "0";
        } else {
            return "null";
        }
    }

    /**
     * Compiles the generated implementation.
     *
     * @param token   type token that was implemented.
     * @param tempDir directory containing the implementation.
     * @throws ImplerException if compilation fails.
     */
    private void compileImplementation(Class<?> token, Path tempDir) throws ImplerException {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("No Java compiler available");
        }

        String implName = token.getSimpleName() + "Impl";
        String packagePath = token.getPackage() != null ?
                token.getPackage().getName().replace('.', File.separatorChar) : "";
        Path sourcePath = tempDir.resolve(packagePath).resolve(implName + ".java");

        String classpath;
        try {
            classpath = Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (URISyntaxException e) {
            throw new ImplerException("Error determining classpath: " + e.getMessage(), e);
        }

        int exitCode = compiler.run(null, null, null,
                "-cp", classpath,
                "-encoding", "UTF-8",
                sourcePath.toString());

        if (exitCode != 0) {
            throw new ImplerException("Compilation failed with exit code: " + exitCode);
        }
    }

    /**
     * Creates a JAR file containing the compiled implementation.
     *
     * @param token   type token that was implemented.
     * @param tempDir directory containing the compiled implementation.
     * @param jarFile path to the target JAR file.
     * @throws ImplerException if JAR creation fails.
     */
    private void createJarFile(Class<?> token, Path tempDir, Path jarFile) throws ImplerException {
        try {
            if (jarFile.getParent() != null) {
                Files.createDirectories(jarFile.getParent());
            }

            Manifest manifest = new Manifest();
            Attributes attributes = manifest.getMainAttributes();
            attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");
            attributes.put(Attributes.Name.MAIN_CLASS, Implementor.class.getName());

            try (JarOutputStream jos = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
                String classFileName = token.getSimpleName() + "Impl.class";
                String packagePath = token.getPackage() != null ?
                        token.getPackage().getName().replace('.', '/') + '/' : "";

                Path classFilePath = tempDir.resolve(packagePath.replace('/', File.separatorChar))
                        .resolve(classFileName);

                if (!Files.exists(classFilePath)) {
                    throw new ImplerException("Compiled class file not found: " + classFilePath);
                }

                JarEntry entry = new JarEntry(packagePath + classFileName);
                jos.putNextEntry(entry);
                Files.copy(classFilePath, jos);
                jos.closeEntry();
            }
        } catch (IOException e) {
            throw new ImplerException("Error creating JAR file: " + e.getMessage(), e);
        }
    }

    /**
     * Deletes a directory and all its contents recursively.
     *
     * @param directory directory to delete.
     * @throws IOException if deletion fails.
     */
    private void deleteDirectory(Path directory) throws IOException {
        Files.walkFileTree(directory, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                Files.delete(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    /**
     * Saves generated code to a file.
     *
     * @param token class for which implementation was generated.
     * @param root root directory.
     * @param className name of the implementation class.
     * @param code generated implementation code.
     * @throws ImplerException if an I/O error occurs while saving the file.
     */
    protected void saveToFile(Class<?> token, Path root, String className, String code) throws ImplerException {
        Path packagePath = root;
        if (token.getPackage() != null) {
            packagePath = root.resolve(token.getPackage().getName().replace('.', File.separatorChar));
        }

        try {
            Files.createDirectories(packagePath);
            Path filePath = packagePath.resolve(className + ".java");
            try (BufferedWriter writer = Files.newBufferedWriter(filePath)) {
                writer.write(handleUnicode(code));
            }
        } catch (IOException e) {
            throw new ImplerException("Error writing to file: " + e.getMessage());
        }
    }

    /**
     * Converts a string to Unicode escape sequences for non-ASCII characters.
     *
     * @param s string to convert
     * @return string with non-ASCII characters replaced with Unicode escape sequences
     */
    protected String handleUnicode(String s) {
        StringBuilder b = new StringBuilder();
        for (char c : s.toCharArray()) {
            if (c >= 128) {
                b.append(String.format("\\u%04X", (int) c));
            } else {
                b.append(c);
            }
        }
        return b.toString();
    }

    /**
     * Represents a method signature, consisting of a method name and parameter types.
     * Used for identifying methods uniquely, regardless of return type and exception types.
     */
    protected static class MethodSignature {
        /**
         * Method name.
         */
        private final String name;

        /**
         * Method parameter types.
         */
        private final Class<?>[] parameterTypes;

        /**
         * Creates a method signature from the given method.
         *
         * @param method method to create signature for.
         */
        public MethodSignature(Method method) {
            this.name = method.getName();
            this.parameterTypes = method.getParameterTypes();
        }

        /**
         * Checks if this method signature equals another object.
         * Two method signatures are equal if they have the same name and parameter types.
         *
         * @param obj object to compare with.
         * @return {@code true} if the objects are equal, {@code false} otherwise.
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (!(obj instanceof MethodSignature that)) return false;
            return name.equals(that.name) && Arrays.equals(parameterTypes, that.parameterTypes);
        }

        /**
         * Returns a hash code for this method signature.
         *
         * @return hash code value for this method signature.
         */
        @Override
        public int hashCode() {
            return 31 * name.hashCode() + Arrays.hashCode(parameterTypes);
        }
    }
}