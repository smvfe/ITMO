#include "entry_lib.h"

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Mochekov Semyon");
MODULE_VERSION("0.01");

struct file_system_type networkfs_fs_type = {
    .owner = THIS_MODULE,
    .name = "networkfs",
    .init_fs_context = networkfs_init_fs_context,
    .kill_sb = networkfs_kill_sb,
};

struct fs_context_operations networkfs_context_ops = {
    .get_tree = networkfs_get_tree,
};

struct inode_operations networkfs_inode_ops = {.lookup = networkfs_lookup,
                                               .create = networkfs_create,
                                               .unlink = networkfs_unlink,
                                               .mkdir = networkfs_mkdir,
                                               .rmdir = networkfs_rmdir,
                                               .link = networkfs_link};

struct file_operations networkfs_dir_ops = {
    .iterate_shared = networkfs_iterate,

};

int networkfs_iterate(struct file *filp, struct dir_context *ctx) {
  unsigned long offset = ctx->pos;
  struct dentry *dentry = filp->f_path.dentry;
  struct inode *inode = dentry->d_inode;
  char *token = inode->i_sb->s_fs_info;

  ino_t ino = inode->i_ino;
  ino_to_string(ino_str, ino);

  loff_t counter = 0;

  struct entries *call_buf = kzalloc(sizeof(struct entries), GFP_KERNEL);
  if (call_buf == NULL) {
    printk(KERN_DEBUG "networkfs_iterate: call_buf allocation error");
    return -ENOMEM;
  }

  int64_t retrn =
      networkfs_http_call(token, "list", (char *)call_buf,
                          sizeof(struct entries), 1, "inode", wstr(ino_str));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_iterate: networkfs_http_call error");
    kfree(call_buf);
    return retrn;
  }

  while (offset < call_buf->entries_count) {
    struct entry ent = call_buf->entries[offset];
    dir_emit(ctx, ent.name, strlen(ent.name), ent.ino, ent.entry_type);
    ctx->pos++;
    offset++;
    counter++;
  }
  kfree(call_buf);
  return counter;
}

struct dentry *networkfs_lookup(struct inode *parent, struct dentry *child,
                                unsigned int flag) {
  struct inode *inode;
  char *token = parent->i_sb->s_fs_info;
  const char *name = child->d_name.name;

  struct entry_info *entr_info = kzalloc(sizeof(struct entry_info), GFP_KERNEL);
  if (!entr_info) {
    kfree(entr_info);
    printk(KERN_ERR "networkfs_lookup: allocation error");
    return NULL;
  }

  ino_t root_ino = parent->i_ino;
  ino_to_string(root_ino_str, root_ino);

  int64_t retrn = networkfs_http_call(token, "lookup", (char *)entr_info,
                                      sizeof(struct entry_info), 2, "parent",
                                      wstr(root_ino_str), "name", wstr(name));

  if (retrn != 0) {
    kfree(entr_info);
    printk(KERN_ERR "networkfs_lookup: networkfs_http_call error");
    return NULL;
  }

  inode = networkfs_get_inode(
      parent->i_sb, NULL, (entr_info->entry_type == DT_DIR ? S_IFDIR : S_IFREG),
      entr_info->ino);
  d_add(child, inode);
  kfree(entr_info);

  return child;
}

int networkfs_create(struct mnt_idmap *idmap, struct inode *parent,
                     struct dentry *child, umode_t mode, bool b) {
  struct inode *inode;
  char *token = parent->i_sb->s_fs_info;

  const char *name = child->d_name.name;
  if (strlen(name) > 255) {
    printk(KERN_ERR "networkfs_create: name too long");
    return -ENAMETOOLONG;
  }

  ino_t root_ino = parent->i_ino;
  ino_to_string(root_ino_str, root_ino);

  ino_t ino_new;
  int64_t retrn = networkfs_http_call(
      token, "create", (char *)&ino_new, sizeof(ino_t), 3, "parent",
      wstr(root_ino_str), "name", wstr(name), "type", wstr("file"));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_create: create_call_error");
    return retrn;
  }

  inode = networkfs_get_inode(parent->i_sb, NULL, S_IFREG | 0x777, ino_new);
  if (inode != NULL) d_add(child, inode);
  return retrn;
}

int networkfs_unlink(struct inode *parent, struct dentry *child) {
  char *token = parent->i_sb->s_fs_info;
  const char *name = child->d_name.name;

  ino_t root_ino = parent->i_ino;
  ino_to_string(root_ino_str, root_ino);

  int64_t retrn = networkfs_http_call(token, "unlink", NULL, 0, 2, "parent",
                                      wstr(root_ino_str), "name", wstr(name));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_unlink: unlink_call_error");
    return retrn;
  }

  return retrn;
}

int networkfs_mkdir(struct mnt_idmap *idmap, struct inode *parent,
                    struct dentry *child, umode_t mode) {
  struct inode *inode;
  char *token = parent->i_sb->s_fs_info;
  const char *name = child->d_name.name;

  ino_t root_ino = parent->i_ino;
  ino_to_string(root_ino_str, root_ino);
  ino_t ino_new;

  int64_t retrn = networkfs_http_call(
      token, "create", (char *)&ino_new, sizeof(ino_t), 3, "parent",
      wstr(root_ino_str), "name", wstr(name), "type", wstr("directory"));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_mkdir: create_call_error");
    return retrn;
  }

  inode = networkfs_get_inode(parent->i_sb, NULL, S_IFDIR | 0x777, ino_new);
  if (inode != NULL) d_add(child, inode);

  return retrn;
}

int networkfs_rmdir(struct inode *parent, struct dentry *child) {
  char *token = parent->i_sb->s_fs_info;
  const char *name = child->d_name.name;

  ino_t root_ino = parent->i_ino;
  ino_to_string(root_ino_str, root_ino);

  int64_t retrn = networkfs_http_call(token, "rmdir", NULL, 0, 2, "parent",
                                      wstr(root_ino_str), "name", wstr(name));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_rmdir: rm_call_error");
    return retrn;
  }

  return retrn;
}

int networkfs_link(struct dentry *target, struct inode *parent,
                   struct dentry *child) {
  struct inode *inode = target->d_inode;
  const char *name = child->d_name.name;
  char *token = parent->i_sb->s_fs_info;

  ino_t parent_ino = parent->i_ino;
  ino_to_string(parent_str, parent_ino);

  ino_t target_ino = inode->i_ino;
  ino_to_string(target_str, target_ino);

  int64_t retrn =
      networkfs_http_call(token, "link", NULL, 0, 3, "source", wstr(target_str),
                          "parent", wstr(parent_str), "name", wstr(name));

  if (retrn != 0) {
    printk(KERN_ERR "networkfs_link: link_call_error");
    return retrn;
  }

  return retrn;
}

/**
 * @sb:     Суперблок файловой системы.
 * @parent: Родительская inode (NULL для корня ФС).
 * @mode:   Битовая маска из прав доступа и типа файла:
 * https://github.com/torvalds/linux/blob/v6.8/include/uapi/linux/stat.h#L9.
 * @i_ino:  Уникальный идентификатор inode.
 */
struct inode *networkfs_get_inode(struct super_block *sb,
                                  const struct inode *parent, umode_t mode,
                                  int i_ino) {
  struct inode *inode = new_inode(sb);

  if (inode != NULL) {
    inode->i_ino = i_ino;
    inode->i_op = &networkfs_inode_ops;
    inode->i_fop = &networkfs_dir_ops;
    inode_init_owner(&nop_mnt_idmap, inode, parent, mode);
  }
  return inode;
}

int networkfs_fill_super(struct super_block *sb, struct fs_context *fc) {
  struct inode *inode = networkfs_get_inode(sb, NULL, S_IFDIR | 0x777, 1000);
  sb->s_root = d_make_root(inode);

  if (sb->s_root == NULL) return -ENOMEM;

  sb->s_fs_info = kmalloc(strlen(fc->source) + 1, GFP_KERNEL);

  if (!sb->s_fs_info) return -ENOMEM;

  strcpy(sb->s_fs_info, fc->source);

  return 0;
}

int networkfs_get_tree(struct fs_context *fc) {
  int ret = get_tree_nodev(fc, networkfs_fill_super);

  if (ret != 0)
    printk(KERN_ERR "networkfs: unable to mount: error code %d", ret);

  return ret;
}

void networkfs_kill_sb(struct super_block *sb) {
  printk(KERN_INFO "networkfs: superblock is destroyed, token: %s",
         (char *)sb->s_fs_info);
  kfree(sb->s_fs_info);
}

int networkfs_init_fs_context(struct fs_context *fc) {
  fc->ops = &networkfs_context_ops;
  return 0;
}

int networkfs_init(void) {
  int error = 0;
  error = register_filesystem(&networkfs_fs_type);
  if (error != 0) {
    printk(KERN_ERR "Failed to register filesystem: %d\n", error);
    return error;
  }
  printk(KERN_INFO "Hello, World!\n");
  return 0;
}

void networkfs_exit(void) {
  int error = 0;

  error = unregister_filesystem(&networkfs_fs_type);
  if (error != 0) {
    printk(KERN_ERR "Failed to unregister filesystem: %d\n", error);
  }

  printk(KERN_INFO "Goodbye World!\n");
}

module_init(networkfs_init);
module_exit(networkfs_exit);
