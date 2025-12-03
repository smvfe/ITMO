package info.kgeorgiy.ja.mochekov.student;

import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.GroupQuery;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements GroupQuery {

    private static final Comparator<Student> STUDENT_COMPARATOR = Comparator.comparing(Student::firstName)
                                                                            .thenComparing(Student::lastName)
                                                                            .thenComparingInt(Student::id);

    private List<Group> getGroupsByPredicate(Collection<Student> students, Comparator<Student> comparator) {
        return collectStream(
                sortedStream(
                        collectStream(openStream(students, Function.identity()),
                                Collectors.groupingBy(Student::groupName))
                                .entrySet().stream()
                                .map(entry ->
                                        new Group(entry.getKey(), sortStudentsByPredicate(entry.getValue(), comparator))),
                        Comparator.comparing(Group::name)
                ),
                Collectors.toList()
        );
    }

    private List<Student> sortStudentsByPredicate(Collection<Student> students, Comparator<Student> comparator) {
        return collectStream(sortedStream(openStream(students, Function.identity()), comparator), Collectors.toList());
    }

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroupsByPredicate(students, STUDENT_COMPARATOR);
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroupsByPredicate(students, Comparator.comparingInt(Student::id));
    }

    @Override
    public GroupName getLargestGroup(Collection<Student> students) {
        return maxStream(
                collectStream(
                        openStream(students, Function.identity()),
                        Collectors.groupingBy(Student::groupName, Collectors.counting())
                ).entrySet().stream(),
                Map.Entry.<GroupName, Long>comparingByValue()
                        .thenComparing(Map.Entry.comparingByKey())
        ).map(Map.Entry::getKey).orElse(null);
    }

    @Override
    public GroupName getLargestGroupFirstName(Collection<Student> students) {
        return maxStream(
                collectStream(
                        openStream(students, Function.identity()),
                        Collectors.groupingBy(Student::groupName, TreeMap::new, Collectors.toList())
                ).entrySet().stream(),
                Comparator.comparingInt(entry ->
                        getDistinctFirstNames(entry.getValue()).size())
        ).map(Map.Entry::getKey).orElse(null);
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return collectStream(openStream(students, Student::firstName), Collectors.toList());
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return collectStream(openStream(students, Student::lastName), Collectors.toList());
    }

    @Override
    public List<GroupName> getGroupNames(List<Student> students) {
        return collectStream(openStream(students, Student::groupName), Collectors.toList());
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return collectStream(openStream(students, this::getFullName), Collectors.toList());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return collectStream(openStream(students, Student::firstName), Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return maxStream(openStream(students, Function.identity()), Comparator.comparingInt(Student::id))
                .map(Student::firstName)
                .orElse("");
    }

    private <T> Optional<T> maxStream(Stream<T> stream, Comparator<? super T> comparator) {
        return stream.max(comparator);
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return collectStream(sortedStream(openStream(students, Function.identity()),
                         Comparator.comparingInt(Student::id)), Collectors.toList());
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return collectStream(sortedStream(openStream(students, Function.identity()), STUDENT_COMPARATOR), Collectors.toList());
    }

    private <R> Stream<R> sortedStream(Stream<R> str, Comparator<? super R> comparator) {
        return str.sorted(comparator);
    }


    private List<Student> findStudentsByPredicate(Collection<Student> students, Predicate<Student> predicate) {
        return collectStream(filterSortedStream(openStream(students, Function.identity()), predicate, STUDENT_COMPARATOR), Collectors.toList());
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentsByPredicate(students, student -> student.firstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentsByPredicate(students, student -> student.lastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentsByPredicate(students, student -> student.groupName().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return filterCollectStream(openStream(students, Function.identity()),
                student -> student.groupName().equals(group),
                Collectors.toMap(
                        Student::lastName,
                        Student::firstName,
                        (firstName1, firstName2) ->
                                firstName1.compareTo(firstName2) < 0 ? firstName1 : firstName2
                ));
    }

    private <R> Stream<R> openStream(Collection<Student> source, Function<Student, R> mapper) {
        return source.stream().map(mapper);
    }

    private <R, A, T> T collectStream(Stream<R> str, Collector<? super R, A, T> collector) {
        return str.collect(collector);
    }

    private <R> Stream<R> filterStream(Stream<R> str, Predicate<? super R> predicate) {
        return str.filter(predicate);
    }

    private <R, A, T> T filterCollectStream(Stream<R> str, Predicate<? super R> predicate, Collector<? super R, A, T> collector) {
        return filterStream(str, predicate).collect(collector);
    }

    private <R> Stream<R> filterSortedStream(Stream<R> str, Predicate<? super R> predicate, Comparator<? super R> comparator) {
        return filterStream(str, predicate).sorted(comparator);
    }

    private String getFullName(Student student) {
        return student.firstName() + " " + student.lastName();
    }
}