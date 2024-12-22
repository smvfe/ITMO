#include <linux/fs.h>
#include <linux/fs_context.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/types.h>

#include "http.h"
#include "util.h"

struct entry_info {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
};

struct entry {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
  char name[256];
};

struct entries {
  size_t entries_count;
  struct entry entries[16];
};

int networkfs_init(void);
void networkfs_exit(void);
struct inode *networkfs_get_inode(struct super_block *, const struct inode *,
                                  umode_t, int);
int networkfs_init_fs_context(struct fs_context *);
int networkfs_fill_super(struct super_block *, struct fs_context *);
int networkfs_get_tree(struct fs_context *);
void networkfs_kill_sb(struct super_block *);
struct dentry *networkfs_lookup(struct inode *, struct dentry *, unsigned int);
int networkfs_iterate(struct file *, struct dir_context *);
int networkfs_create(struct mnt_idmap *, struct inode *, struct dentry *,
                     umode_t, bool);
int networkfs_unlink(struct inode *, struct dentry *);
int networkfs_mkdir(struct mnt_idmap *, struct inode *, struct dentry *,
                    umode_t);
int networkfs_rmdir(struct inode *, struct dentry *);
int networkfs_link(struct dentry *, struct inode *, struct dentry *);