#ifndef NETWORKFS_UTIL
#define NETWORKFS_UTIL

#define NFS_SUCCESS 0
#define NFS_NOTFOUND 1
#define NFS_NOTAFILE 2
#define NFS_NOTADIR 3
#define NFS_NOENTRY 4
#define NFS_DUPENTRY 5
#define NFS_BIGFILE 6
#define NFS_BIGDIR 7
#define NFS_NOTEMPTY 8
#define NFS_BIGNAME 9

#define ino_to_string(var, src) \
  char var[19];                 \
  sprintf(var, "%lu", (src));

#define wstr(var) (var), strlen(var)

#endif
