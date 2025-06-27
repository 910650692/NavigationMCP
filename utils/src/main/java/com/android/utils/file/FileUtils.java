package com.android.utils.file;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.content.res.AssetManager;
import android.os.Environment;
import android.text.TextUtils;
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.security.PublicKey;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/21
 */
public class FileUtils {
    private static final String TAG = FileUtils.class.getSimpleName();
    private Context mContext;

    private FileUtils() {

    }

    public void initFile(Context context) {
        this.mContext = context;
    }

    public boolean checkFile(String filepath) {
        if (ConvertUtils.isEmpty(filepath)) return false;
        File file = new File(filepath);
        return checkFile(file);
    }

    public boolean checkFileDir(String dirPath) {
        if (ConvertUtils.isEmpty(dirPath)) return false;
        File file = new File(dirPath);
        return checkFileDir(file);
    }

    public boolean createFile(String filepath) {
        return createFile(filepath, false);
    }

    public boolean createFile(String filepath, boolean forceRecreate) {
        return createFile(new File(filepath), forceRecreate);
    }

    public boolean createDir(String dirPath) {
        return createDir(dirPath, false);
    }

    public boolean createDir(String dirPath, boolean forceRecreate) {
        if (ConvertUtils.isEmpty(dirPath)) return false;
        File file = new File(dirPath);
        return createDir(file, forceRecreate);
    }

    public boolean deleteFile(String path) {
        if (ConvertUtils.isEmpty(path)) return false;
        return deleteFile(new File(path));
    }

    public boolean deleteDir(String dir) {
        if (ConvertUtils.isEmpty(dir)) return false;
        return deleteDir(new File(dir));
    }

    public void writeMsg(String msg, String fileRelPathAndName, boolean append) {
        writeToFile(msg, fileRelPathAndName, append);
    }

    public void writeByteArr(byte[] byteArr, String fileRelPathAndName, boolean append) {
        writeToFile(byteArr, fileRelPathAndName, append);
    }

    public void copyAssetsFolder(String assetsRelPath, String destPath) {
        copyAssetsFolder(assetsRelPath, destPath, false);
    }

    public void copyAssetsFolders(String assetsRelPath, String destPath) {
        copyAssetsFolders(assetsRelPath, destPath, false);
    }

    /**
     * 检查文件是否存在.
     *
     * @param filepath Detected files
     * @return true 存在/false 不存在
     */
    public boolean checkFile(File filepath) {
        if (ConvertUtils.isEmpty(filepath)) return false;
        return filepath.isFile() && filepath.exists();
    }

    /**
     * 检查文件路径是否存在.
     *
     * @param dirPath Detected file path
     * @return true 存在/false 不存在
     */
    public boolean checkFileDir(File dirPath) {
        if (ConvertUtils.isNull(dirPath)) return false;
        return dirPath.isDirectory() && dirPath.exists();
    }

    /**
     * 创建文件
     *
     * @param file          File path
     * @param forceRecreate 是否强制创建
     * @return true 创建成功/false 创建失败
     */
    private boolean createFile(File file, boolean forceRecreate) {
        try {
            if (ConvertUtils.isNull(file)) return false;
            boolean isExist = checkFile(file);
            if (isExist) {
                if (forceRecreate) deleteFile(file);
                else return true;
            }
            createDir(file.getParent(), false);
            return file.createNewFile();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 创建文件目录.
     *
     * @param dirPath       File path
     * @param forceRecreate 是否强制创建
     * @return true 创建成功/false 创建失败
     */
    public boolean createDir(File dirPath, boolean forceRecreate) {
        if (ConvertUtils.isEmpty(dirPath)) return false;

        boolean isExist = checkFileDir(dirPath);
        if (isExist) {
            if (forceRecreate) deleteDir(dirPath);
            else return true;
        }
        return dirPath.mkdirs();
    }

    /**
     * 删除文件.
     *
     * @param path File path
     * @return true 删除成功/false 删除失败
     */
    public boolean deleteFile(File path) {
        if (!checkFile(path)) return true;
        return path.delete();
    }

    /**
     * 删除指定文件夹下的所有文件和子文件夹
     *
     * @param dir 文件夹路径
     * @return 是否删除成功
     */
    public static boolean deleteFilesInDirectory(final File dir) {
        if (dir.exists() && dir.isDirectory()) {
            final File[] files = dir.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        deleteFilesInDirectory(file); // 递归删除子文件夹
                    } else {
                        file.delete(); // 删除文件
                    }
                }
            }
            return true;
        }
        return false;
    }

    /**
     * 删除多个文件夹下的所有文件和子文件夹
     *
     * @param dirs 文件夹路径数组
     * @return 是否删除成功
     */
    public static boolean deleteFilesInDirectories(final File[] dirs) {
        if (dirs == null || dirs.length == 0) return false;

        for (File dir : dirs) {
            if (dir.exists() && dir.isDirectory()) {
                if (!deleteFilesInDirectory(dir)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * 获取指定文件夹下的所有文件的总大小
     *
     * @param dir 文件夹路径
     * @return 文件夹大小（字节）
     */
    public static long getDirectorySize(final File dir) {
        long size = 0;
        if (dir.exists() && dir.isDirectory()) {
            final File[] files = dir.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        size += getDirectorySize(file); // 递归获取子文件夹大小
                    } else {
                        size += file.length(); // 累加文件大小
                    }
                }
            }
        }
        return size;
    }

    /**
     * 获取多个文件夹下的所有文件的总大小
     *
     * @param dirs 文件夹路径数组
     * @return 文件夹总大小（字节）
     */
    public static long getTotalSizeOfDirectories(final File[] dirs) {
        if (dirs == null || dirs.length == 0) return 0;

        long totalSize = 0;
        for (File dir : dirs) {
            if (dir.exists() && dir.isDirectory()) {
                totalSize += getDirectorySize(dir);
            }
        }
        return totalSize;
    }


    /**
     * 格式化文件大小为指定格式
     *
     * @param size 文件大小（字节）
     * @return 格式化后的字符串
     */
    @SuppressLint("DefaultLocale")
    public static String formatFileSize(final long size) {
        if (size <= 0) {
            return "0KB";
        }

        final double KB = 1024;
        final double MB = KB * 1024;
        final double GB = MB * 1024;

        if (size < KB) {
            return size + "B";
        } else if (size < MB) {
            return String.format("%.0fKB", size / KB);
        } else if (size < GB) {
            return String.format("%.1fM", size / MB);
        } else {
            return String.format("%.2fG", size / GB);
        }
    }

    /**
     * 删除文件路径.
     *
     * @param dir 文件路径
     * @return 操作结果
     */
    private boolean deleteDir(File dir) {
        if (!checkFileDir(dir)) return true;
        File[] var1 = dir.listFiles();
        int var2 = var1.length;
        for (int var3 = 0; var3 < var2; ++var3) {
            File file = var1[var3];
            if (file.isFile()) {
                file.delete();
            } else if (file.isDirectory()) {
                deleteDir(file);
            }
        }
        return dir.delete();
    }

    /**
     * 获取文件输入流.
     *
     * @param filePath 文件路径
     * @return 文件的输入流
     */
    public byte[] getFileInputStream(String filePath) {
        byte[] buffer = null;
        createFile(filePath);
        InputStream inputStream = null;
        File file = new File(filePath);
        try {
            inputStream = new FileInputStream(file);
            buffer = new byte[inputStream.available()];
            inputStream.read(buffer);
        } catch (IOException e) {
            if (Logger.openLog) {
                Logger.d(TAG, "[copyFile] e = {?}", Log.getStackTraceString(e));
            }
        } finally {
            safetyClose(inputStream);
        }
        return buffer;
    }


    private void writeToFile(String msg, String fileRelPathAndName, boolean append) {
        if (ConvertUtils.isEmpty(msg) || ConvertUtils.isEmpty(fileRelPathAndName))
            throw new RuntimeException("write file msg or file path is null");
        FileWriter fileWriter = null;
        BufferedWriter bufferedWriter = null;
        try {
            createFile(fileRelPathAndName);
            File file = new File(fileRelPathAndName);
            fileWriter = new FileWriter(file, append);
            bufferedWriter = new BufferedWriter(fileWriter);
            bufferedWriter.write(msg + "\r\n");
            bufferedWriter.flush();
        } catch (IOException var10) {
            var10.printStackTrace();
        } finally {
            safetyClose(bufferedWriter);
            safetyClose(fileWriter);
        }
    }

    private void writeToFile(byte[] byteArr, String fileRelPathAndName, boolean append) {
        if (ConvertUtils.isEmpty(byteArr) || ConvertUtils.isEmpty(fileRelPathAndName))
            throw new RuntimeException("write file msg or file path is null");
        FileOutputStream fos = null;
        try {
            createFile(fileRelPathAndName);
            fos = new FileOutputStream(fileRelPathAndName, append);
            fos.write(byteArr);
            fos.flush();
        } catch (IOException var9) {
            var9.printStackTrace();
        } finally {
            safetyClose(fos);
        }
    }

    /**
     * 列出指定路径目录的所有子文件列表(只包含一级子文件)
     * 若所给路径并未表示目录, 则返回空数据
     *
     * @param folderPath 目录路径
     */
    @Nullable
    public File[] listSubFiles(String folderPath) {
        if (TextUtils.isEmpty(folderPath)) {
            return null;
        }
        File folder = new File(folderPath);
        boolean isDir = folder.exists() && folder.isDirectory();
        if (!isDir) {
            return null;
        }
        return folder.listFiles();
    }

    /**
     * 复制文件
     *
     * @param targetFile 为目标文件
     * @param file       为源文件
     */
    public void copyFile(File file, File targetFile) {
        if (targetFile.exists()) {

        } else {
            File parentFile = new File(targetFile.getParent() + "/");
            parentFile.mkdirs();
            createFile(targetFile, true);
        }
        InputStream is = null;
        FileOutputStream fos = null;
        try {
            is = new FileInputStream(file);
            fos = new FileOutputStream(targetFile);
            byte[] buffer = new byte[1024];
            while (is.read(buffer) != -1) {
                fos.write(buffer);
            }
        } catch (IOException e) {
            if (Logger.openLog) {
                Logger.d(TAG, "[copyFile] e = {?}", Log.getStackTraceString(e));
            }
        } finally {
            safetyClose(is);
            safetyClose(fos);
        }
    }

    /**
     * 拷贝单个文件
     */
    public boolean copyFileBuffered(File source, File target, FileUtils.FileListener listener) {
        boolean result = false;
        if (!target.exists()) {
            FileInputStream in = null;
            FileOutputStream out = null;
            try {
                in = new FileInputStream(source);
                out = new FileOutputStream(target);
                byte[] buffer = new byte[1024];
                int length;
                while ((length = in.read(buffer)) != -1) {
                    if (listener == null) {
                        Logger.i(TAG, "用户关闭了控制面板");
                        break;
                    } else if (listener.isCancelled()) {
                        Logger.i(TAG, "用户取消了复制");
                        break;
                    }
                    out.write(buffer, 0, length);
                }
                out.flush();
                out.getFD().sync();
                result = true;
            } catch (IOException e) {
                if (Logger.openLog) {
                    Logger.d(TAG, "[copyFileBuffered] e = {?}", Log.getStackTraceString(e));
                }
            } finally {
                safetyClose(in);
                safetyClose(out);
            }
        }
        return result;
    }

    /**
     * 异步拷贝文件夹
     * @param addTime 是否添加时间戳到目标路径
     * @param fromPath 源路径
     * @param toPath 目标路径
     * @param listener 监听器，用于接收复制进度和状态（为空不执行）
     */
    public void copyFileDirectory(boolean addTime, String fromPath, String toPath, FileUtils.FileListener listener) {
        try {
            if (TextUtils.isEmpty(fromPath)) {
                if (listener != null) {
                    listener.onCopyInfo("源路径为空");
                }
                return;
            }
            // 源目录：（需确保应用有读取权限）
            File sourceDir = new File(fromPath);
            if (!sourceDir.exists() || !sourceDir.isDirectory()) {
                if (listener != null) {
                    listener.onCopyInfo("源目录不存在:" + fromPath);
                }
                return;
            }
            // 目标目录：（自动创建）
            if (TextUtils.isEmpty(toPath)) {
                if (listener != null) {
                    listener.onCopyInfo("目标路径为空");
                }
                return;
            }
            File targetDir = null;
            if (addTime) {
                // 添加时间戳到目标路径
                @SuppressLint("SimpleDateFormat")
                String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
                targetDir = new File(toPath, sourceDir.getName() + timeStamp);
            } else {
                targetDir = new File(toPath, sourceDir.getName());
            }
            if (!targetDir.exists() && !targetDir.mkdirs()) {
                if (listener != null) {
                    listener.onCopyInfo("目标目录创建失败");
                }
                return;
            }
            // 拷贝所有文件
            File[] files = sourceDir.listFiles();
            if (files == null) {
                if (listener != null) {
                    listener.onCopyInfo("源目录无文件");
                }
                return;
            }
            for (File file : files) {
                if (listener == null) {
                    Logger.i(TAG, "用户关闭了控制面板");
                    break;
                } else if (listener.isCancelled()) {
                    Logger.i(TAG, "用户取消了复制");
                    listener.onCopyInfo("用户取消了复制");
                    break;
                }
                if (file.isFile()) {
                    listener.onCopyInfo(file.getName());
                    File targetFile = new File(targetDir, file.getName());
                    boolean result = copyFileBuffered(file, targetFile, listener);
                } else if (file.isDirectory()) {
                    // 递归拷贝子目录
                    copyFileDirectory(false, file.getAbsolutePath(),
                            new File(targetDir, file.getName()).getAbsolutePath(), listener);
                }
            }
        } catch (Exception e) {
            if (listener != null) {
                listener.onCopyInfo("复制出错");
            }
            if (Logger.openLog) {
                Logger.e(TAG, e.getMessage());
            }
            e.printStackTrace();
        }
    }

    /**
     * 复制Asset文件夹到内存中.
     *
     * @param assetsRelPath          asset文件夹目录
     * @param destPath               要复制到的路径
     * @param recreateDestFileIfNeed 是否清除已存在的目录
     */
    public void copyAssetsFolders(String assetsRelPath, String destPath, boolean recreateDestFileIfNeed) {
        if (ConvertUtils.isEmpty(assetsRelPath))
            throw new RuntimeException("copyAssetsFolder fail as srcFile path is empty,assetsRelPath:" + assetsRelPath);
        if (ConvertUtils.isEmpty(destPath))
            throw new RuntimeException("copyAssetsFolder fail as srcFile path is empty,destPath:" + destPath);
        if (null == mContext) return;
        if (!recreateDestFileIfNeed && checkFileDir(destPath)) return;
        createDir(destPath, recreateDestFileIfNeed);
        AssetManager assetManager = mContext.getAssets();
        try {
            if (assetsRelPath.endsWith("/"))
                assetsRelPath = assetsRelPath.substring(0, assetsRelPath.length() - 1);
            String[] fileNames = assetManager.list(assetsRelPath);
            if (ConvertUtils.isEmpty(fileNames))
                copyAssetsFolder(assetsRelPath, destPath, recreateDestFileIfNeed);
            for (String fileName : fileNames) {
                String[] childFileNames = assetManager.list(assetsRelPath + "/" + fileName);
                if (ConvertUtils.isEmpty(childFileNames))
                    copyAssetsFolder(assetsRelPath + "/" + fileName, destPath + "/" + fileName, recreateDestFileIfNeed);
                else
                    copyAssetsFolders(assetsRelPath + "/" + fileName, destPath + "/" + fileName, recreateDestFileIfNeed);
            }
        } catch (Exception var12) {
            throw new RuntimeException("复制asset文件出错: " + assetsRelPath + " " + var12.getMessage());
        }
    }

    /**
     * 复制Asset文件夹到内存中.
     *
     * @param assetsRelPath          asset文件路径
     * @param destPath               要保存的文件
     * @param recreateDestFileIfNeed 是否清除已存在的文件
     */
    public void copyAssetsFolder(String assetsRelPath, String destPath, boolean recreateDestFileIfNeed) {
        if (ConvertUtils.isEmpty(assetsRelPath))
            throw new RuntimeException("copyAssetsFolder fail as srcFile path is empty,assetsRelPath:" + assetsRelPath);
        if (ConvertUtils.isEmpty(destPath))
            throw new RuntimeException("copyAssetsFolder fail as srcFile path is empty,destPath:" + destPath);
        if (null == mContext) return;
        if (!recreateDestFileIfNeed && checkFile(destPath)) return;
        createFile(destPath, recreateDestFileIfNeed);
        AssetManager assetManager = mContext.getAssets();
        try {
            InputStream is = assetManager.open(assetsRelPath);
            FileOutputStream fos = new FileOutputStream(destPath);
            byte[] buffer = new byte[1024];
            int byteCount;
            while ((byteCount = is.read(buffer)) != -1) {
                fos.write(buffer, 0, byteCount);
            }
            fos.flush();
            safetyClose(is);
            safetyClose(fos);
        } catch (Exception e) {
            throw new RuntimeException("复制asset文件出错: " + assetsRelPath + " " + e.getMessage());
        }
    }

    /**
     * 获取Asset文件的输出流.
     *
     * @param assetFileRelPath asset文件的路径
     * @return 字节数组
     */
    public byte[] getAssetFileContent(String assetFileRelPath) {
        if (mContext == null) {
            return new byte[0]; // 返回空数组代替 null，避免空指针
        }
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try (InputStream inputStream = mContext.getAssets().open(assetFileRelPath)) {
            byte[] buffer = new byte[1024];
            int length;
            while ((length = inputStream.read(buffer)) != -1) {
                byteArrayOutputStream.write(buffer, 0, length);
            }
            return byteArrayOutputStream.toByteArray();
        } catch (IOException ioException) {
            Logger.e("TAG", "读取文件异常", ioException);
            return new byte[0]; // 出错也返回空数组保持一致性
        } finally {
            safetyClose(byteArrayOutputStream);
        }
    }

    public boolean isAssetFile(String assetPath) {
        AssetManager assetManager = mContext.getAssets();
        try {
            assetManager.openFd(assetPath);
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    public void safetyClose(Closeable closeable) {
        try {
            if (closeable == null) return;
            closeable.close();
        } catch (IOException exception) {
            Logger.e(TAG, exception.getMessage());
        }
    }

    /**
     * 设置目录及文件权限为 777
     *
     * @param dirPath 目录
     * @return 是否成功
     */
    public boolean setFullPermissions(String dirPath) {
        if (ConvertUtils.isEmpty(dirPath)) {
            if (Logger.openLog) {
                Logger.i(TAG, "initEngineParam dirPath isEmpty");
            }
            return false;
        }
        try {
            File file = new File(dirPath);
            // 设置可读、可写、可执行权限给所有者、组和其他用户
            @SuppressLint("SetWorldReadable")
            boolean readable = file.setReadable(true, false);
            @SuppressLint("SetWorldWritable")
            boolean writable = file.setWritable(true, false);
            boolean executable = file.setExecutable(true, false);
            if (Logger.openLog) {
                Logger.i(TAG, "initEngineParam isDirectory：", file.isDirectory(), " isFile:", file.isFile(), " exists:", file.exists()
                        , " readable:", readable, " writable:", writable, " executable:", executable, " dirPath:", dirPath);
            }
            return readable && writable && executable;
        } catch (Exception e) {
            Logger.e(TAG, "initEngineParam error：" + e.getMessage());
            return false;
        }
    }

    public void close() {
        mContext = null;
    }

    /**
     * 应用内部缓存目录
     * <p>
     * 内部沙箱位置：/data/user/0/your_package/cache
     * </P>
     */
    public String getAPPCachePath() {
        String appCachePath = mContext.getCacheDir().getAbsolutePath() + File.separator;
        Logger.i(TAG, "APP_CACHE_PATH", appCachePath);
        return appCachePath;
    }

    /**
     * 应用内部缓存目录
     * <p>
     * 内部沙箱位置：/data/data/0/your_package/files
     * </P>
     */
    public String getAppFilePath() {
        String appFilePath = mContext.getFilesDir().getAbsolutePath() + File.separator;
        Logger.i(TAG, "APP_FILE_PATH", appFilePath);
        return appFilePath;
    }

    /**
     * 应用外部缓存目录
     * <p>
     * 外部沙箱位置：sdcard/Android/data/your_package/files
     * </P>
     */
    public String getEmulatedPhonePath() {
        String sdAppFilePath = mContext.getExternalFilesDir(null) + File.separator;
        Logger.i(TAG, "SD_APP_FILE_PATH", sdAppFilePath);
        return sdAppFilePath;
    }

    /**
     * 外部存储位置
     * <p>
     * /storage/emulated/0
     * </P>
     */
    public String getEmulatedSDPath() {
        String sdCardPath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator;
        Logger.i(TAG, "SD_PATH", sdCardPath);
        return sdCardPath;
    }

    public boolean isStorageAreaAvailable() {
        boolean permission = checkExternalStoragePermission();
        boolean storagePath = checkExternalStorageAvailable();
        Logger.i(TAG, "storagePath hasPermission:", permission, "storagePath isAvailable:", storagePath);
        return permission && storagePath;
    }

    /**
     * @return 是否有外部存储 true:有 false:没有
     */
    public boolean checkExternalStorageAvailable() {
        String state = Environment.getExternalStorageState();
        if (!Environment.MEDIA_MOUNTED.equals(state)) {
            Logger.i(TAG, "MNT partition was mounted fail");
            return false;
        }
        Logger.i(TAG, "MNT partition was mounted successfully");
        File file = mContext.getExternalFilesDir(null);
        return checkFileDir(file);
    }

    /**
     * @return 是否有权限访问外部存储
     */
    private boolean checkExternalStoragePermission() {
        boolean externalStoragePermissionCheck = Environment.isExternalStorageManager();
        Logger.i(TAG, "externalStoragePermissionCheck:", externalStoragePermissionCheck);
        return externalStoragePermissionCheck;
    }

    public static FileUtils getInstance() {
        return Helper.fu;
    }

    private static final class Helper {
        private static final FileUtils fu = new FileUtils();
    }

    public interface FileListener {
        /**
         * 文件复制信息回调
         *
         * @param info
         */
        default void onCopyInfo(String info) {
        }

        /**
         * 复制完成
         */
        default void onComplete() {
        }

        /**
         * 是否取消复制
         *
         * @return true:取消复制，false:继续复制
         */
        default boolean isCancelled() {
            return false;
        }
    }
}
