package com.fy.navi.service.adapter.engine.bls;

import android.widget.Toast;

import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.GBLCacheFilePath;

import java.io.File;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class SdkSoLoadUtils {
    private static final String TAG = MapDefaultFinalTag.ENGINE_SERVICE_TAG;

    public static boolean loadDebugSo(File soFile) {
        boolean isException = false;
        String libName = soFile.getName();
        //本地存在debug so，则加载本地so
        File oldFile = new File(getLibsCopyPath() + "/" + libName);
        if (oldFile.exists()) {
            File delFile = new File(getLibsCopyPath() + "/"
                    + libName + System.currentTimeMillis());
            if (oldFile.renameTo(delFile)) {
                delFile.delete();
            }
        }
        String debugSoPath = getLibsCopyPath() + "/lib" + libName + ".so";
        FileUtils.getInstance().copyFile(soFile, new File(debugSoPath));
        try {
            System.load(debugSoPath);
        } catch (Throwable e) {
            ThreadManager.getInstance().postUi(() ->
                    Toast.makeText(AppContext.getInstance().getMContext(), "libs加载外部so库崩溃", Toast.LENGTH_LONG));
            isException = true;
        } finally {
            if (!isException) {
                final String message = "加载外部so库-" + libName;
                ThreadManager.getInstance().postUi(() ->
                        Toast.makeText(AppContext.getInstance().getMContext(), message, Toast.LENGTH_LONG));
            }
        }
        return isException;
    }

    private static String getLibsCopyPath() {
        return "/data/data/" + AppContext.getInstance().getMApplication().getPackageName() + "/sdkLibs";
    }

    public static void loadLibrary() {
        Logger.i(TAG, GBLCacheFilePath.DEBUG_LIBS_DIR);
        if (FileUtils.getInstance().checkFileDir(GBLCacheFilePath.DEBUG_LIBS_DIR)) {
            //先判断debug so中是否有gbl，如果有，gbl需要最后加载
            File[] listFile = FileUtils.getInstance().listSubFiles(GBLCacheFilePath.DEBUG_LIBS_DIR);
            if (listFile == null) {
                Logger.i(TAG, "文件夹存在，但是没有内容");
                return;
            }
            for (int i = 0; i < listFile.length; i++) {
                File soFile = listFile[i];
                Logger.i(TAG, "遍历 listFile -> " + soFile.getName());
                if ("libGbl.so".equals(soFile.getName())) {
                    continue;
                }
                loadDebugSo(soFile);
            }
            File gblSoFile = new File(GBLCacheFilePath.DEBUG_LIBS_DIR + "/libGbl.so");
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

    public static int copyAssetsFiles() {
        // 若 amapauto20/ 目录创建失败, 可能导致 bl 初始化失败

        //卡片服务
/*        Logger.i(TAG, "copy 卡片服务", GBLCacheFilePath.COPY_ASSETS_DIR + "CardRes/images");
        FileUtils.getInstance().copyAssetsFolders("images",
                GBLCacheFilePath.COPY_ASSETS_DIR + "CardRes/images", true);

        Logger.i(TAG, "copy dynamic", GBLCacheFilePath.COPY_ASSETS_DIR + "dynamic/layers");
        FileUtils.getInstance().copyAssetsFolders("dynamic/layers",
                GBLCacheFilePath.COPY_ASSETS_DIR + "dynamic/layers", true);*/

        // 自定义纹理使用，HMI自行修改维护
//        Logger.i(TAG, "copy 自定义纹理", GBLCacheFilePath.COPY_ASSETS_DIR);
//        FileUtils.getInstance().copyAssetsFolders("bls/style1",
//                GBLCacheFilePath.COPY_ASSETS_DIR + "bls/style1");

/*         //使用HMI 自己的字体，需要自行拷贝管理，如果是使用默认字体，则HMI无需自己拷贝
        Logger.i(TAG, "copy 字体风格", descDirPath + "font");
        FileUtils.getInstance().copyAssetsFolders("blRes/font",
                descDirPath + "font", true);

       // 车道级导航回放文件
        Logger.i(TAG, "copy 车道级导航回放文件", GBLCacheFilePath.GPS_LANELOC_FOLDER);
        FileUtils.getInstance().copyAssetsFolders("lanetest/loc_replay/", GBLCacheFilePath.GPS_LANELOC_FOLDER);

        // 复制仿真回放数据recorder
        Logger.i(TAG, "copy 复制仿真回放数据recorder", GBLCacheFilePath.RECORDER_DATA_DIR);
        FileUtils.getInstance().copyAssetsFolder("lanetest/recorder/", GBLCacheFilePath.RECORDER_DATA_DIR);

        // 复制车道级车标,引导线等资源文件(329之后需要hmi自行提供, aar包中不内置)
        Logger.i(TAG, "copy 制车道级车标,引导线等资源文件", GBLCacheFilePath.OFFLINE_LANERESOURCE_DIR);
        FileUtils.getInstance().copyAssetsFolder("bl_lane_Res/LaneCarSRResource", GBLCacheFilePath.OFFLINE_LANERESOURCE_DIR);*/
        return 0;
    }
}
