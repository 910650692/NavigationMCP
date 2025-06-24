package com.sgm.navi.service.adapter.engine.bls;

import android.widget.Toast;

import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;

import java.io.File;

/**
 * TODO说明
 * @author lvww
 * @version  $Revision.2024/11/24$
 */
public final class SdkSoLoadUtils {
    private static final String TAG = MapDefaultFinalTag.ENGINE_SERVICE_TAG;

    /**
     * loadDebugSo
     *
     * @param soFile File
     */
    public static void loadDebugSo(final File soFile) {
        boolean isException = false;
        final String libName = soFile.getName();
        //本地存在debug so，则加载本地so
        final File oldFile = new File(getLibsCopyPath() + "/" + libName);
        if (oldFile.exists()) {
            final File delFile = new File(getLibsCopyPath() + "/"
                    + libName + System.currentTimeMillis());
            if (oldFile.renameTo(delFile)) {
                delFile.delete();
            }
        }
        final String debugSoPath = getLibsCopyPath() + "/lib" + libName + ".so";
        FileUtils.getInstance().copyFile(soFile, new File(debugSoPath));
        try {
            System.load(debugSoPath);
        } catch (UnsatisfiedLinkError | SecurityException | NullPointerException e) {
            ThreadManager.getInstance().postUi(() ->
                    Toast.makeText(AppCache.getInstance().getMContext(), "libs加载外部so库崩溃", Toast.LENGTH_LONG));
            isException = true;
        } finally {
            if (!isException) {
                final String message = "加载外部so库-" + libName;
                ThreadManager.getInstance().postUi(() ->
                        ToastUtils.Companion.getInstance().showCustomToastView(message));
            }
        }
    }

    private static String getLibsCopyPath() {
        return "/data/data/" + AppCache.getInstance().getMApplication().getPackageName() + "/sdkLibs";
    }

    /**
     * 加载库
     */
    public static void loadLibrary() {
        Logger.i(TAG, GBLCacheFilePath.DEBUG_LIBS_DIR);
        if (FileUtils.getInstance().checkFileDir(GBLCacheFilePath.DEBUG_LIBS_DIR)) {
            //先判断debug so中是否有gbl，如果有，gbl需要最后加载
            final File[] listFile = FileUtils.getInstance().listSubFiles(GBLCacheFilePath.DEBUG_LIBS_DIR);
            if (listFile == null) {
                Logger.i(TAG, "文件夹存在，但是没有内容");
                return;
            }
            for (final File soFile : listFile) {
                Logger.i(TAG, "遍历 listFile -> " + soFile.getName());
                if ("libGbl.so".equals(soFile.getName())) {
                    continue;
                }
                loadDebugSo(soFile);
            }
            final File gblSoFile = new File(GBLCacheFilePath.DEBUG_LIBS_DIR + "/libGbl.so");
            Logger.i(TAG, "gblSoFile -> " + gblSoFile.getPath());
            if (gblSoFile.exists()) {
                loadDebugSo(gblSoFile);
            } else {
                System.loadLibrary("Gbl");
            }
        } else {
            Logger.i(TAG, "不存在 加载新内容");
            System.loadLibrary("Gbl");
        }
    }
}
