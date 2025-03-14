package com.android.utils.file;

import android.content.Context;
import android.content.res.AssetManager;
import android.os.Environment;
import android.text.TextUtils;
import android.util.Log;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/21
 */
public class FileUtils {
    private static final String TAG = FileUtils.class.getSimpleName();

    private static final int FILE_BUFFER = 1024;

    private static final int QUANTITY_VALUE = 100;

    private static final int DATA_BYTE = 2048;

    private static final int LENGHT_LIMIT = 3;

    public static String SD_PATH;
    public static String SD_APP_PATH;
    public static String APP_FILE_PATH;
    public static String APP_CACHE_PATH;
    private Context mContext;

    private FileUtils() {

    }

    public void initFile(Context context) {
        this.mContext = context;
        getEmulatedPhonePath();
        getEmulatedSDPath();
        getAPPCachePath();
        getAppFilePath();
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
     * @param filepath
     * @return
     */
    public boolean checkFile(File filepath) {
        if (ConvertUtils.isEmpty(filepath)) return false;
        return filepath.isFile() && filepath.exists();
    }

    /**
     * 检查文件路径是否存在.
     *
     * @param dirPath
     * @return
     */
    public boolean checkFileDir(File dirPath) {
        if (ConvertUtils.isNull(dirPath)) return false;
        return dirPath.isDirectory() && dirPath.exists();
    }

    /**
     * 创建文件
     *
     * @param file
     * @param forceRecreate
     * @return
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
     * @param dirPath
     * @param forceRecreate
     * @return
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
     * @param path
     * @return
     */
    public boolean deleteFile(File path) {
        if (!checkFile(path)) return true;
        return path.delete();
    }

    /**
     * 删除文件路径.
     *
     * @param dir
     * @return
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
     * @param filePath
     * @return
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
            Logger.d(TAG, "[copyFile] e = {?}", Log.getStackTraceString(e));
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
     * 复制文件。targetFile为目标文件，file为源文件
     *
     * @param targetFile
     * @param file
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

            Logger.d(TAG, "[copyFile] e = {?}", Log.getStackTraceString(e));
        } finally {
            safetyClose(is);
            safetyClose(fos);
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
            if (ConvertUtils.isEmpty(fileNames)) copyAssetsFolder(assetsRelPath, destPath, recreateDestFileIfNeed);
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
            return null;
        } else {
            InputStream inputStream = null;
            byte[] result = null;
            try {
                inputStream = mContext.getAssets().open(assetFileRelPath);
                result = new byte[inputStream.available()];
                inputStream.read(result);
            } catch (IOException ioException) {
                Logger.e(ioException.toString(), new Object[0]);
            } finally {
                safetyClose(inputStream);
            }
            return result;
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
        } catch (IOException var2) {
            var2.printStackTrace();
        }
    }

    public void close() {
        mContext = null;
    }

    /**
     * 应用内部缓存目录
     *
     * @return 内部沙箱位置：/data/user/0/your_package/cache
     */
    private void getAPPCachePath() {
        APP_CACHE_PATH = mContext.getCacheDir().getAbsolutePath() + File.separator;
    }

    /**
     * 应用内部缓存目录
     * @return 内部沙箱位置：/data/data/0/your_package/files
     */
    private void getAppFilePath() {
        APP_FILE_PATH = mContext.getFilesDir().getAbsolutePath() + File.separator;
    }

    /**
     * 应用外部缓存目录
     * @return 外部沙箱位置：sdcard/Android/data/your_package/files
     */
    private void getEmulatedPhonePath() {
        SD_APP_PATH = mContext.getExternalFilesDir(null) + File.separator;
    }

    /**
     * 外部存储位置
     *
     * @return /storage/emulated/0
     */
    private void getEmulatedSDPath() {
        SD_PATH = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator;
    }

    public static FileUtils getInstance() {
        return Helper.fu;
    }

    private static final class Helper {
        private static final FileUtils fu = new FileUtils();
    }
}
