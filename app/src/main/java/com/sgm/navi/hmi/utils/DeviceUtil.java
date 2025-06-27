package com.sgm.navi.hmi.utils;

import android.content.Context;
import android.os.storage.StorageManager;
import android.os.storage.StorageVolume;

import com.android.utils.log.Logger;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description 设备相关工具类
 * 用于获取USB存储设备的路径
 * @Author changshan.jiang
 * @date 2024/11/21
 */
public class DeviceUtil {
    private static final String TAG =  "DeviceUtil";

    /**
     * 获取USB存储设备的路径
     * @param context
     * @return USB存储设备的路径列表
     */
    public static List<String> getUSBPath(Context context) {
        List<String> usbPaths = new ArrayList<>();
        try {
            StorageManager storageManager = (StorageManager) context.getSystemService(Context.STORAGE_SERVICE);
            List<StorageVolume> volumeList = storageManager.getStorageVolumes();
            if (volumeList.isEmpty()) {
                Logger.e(TAG, "没有找到USB存储设备");
                return usbPaths;
            }
            for (StorageVolume volume : volumeList) {
                if (volume != null && volume.isRemovable()) {
                    if(volume.getDirectory() != null) {
                        usbPaths.add(volume.getDirectory().getAbsolutePath());
                    }
                }
            }
        } catch (Exception e) {
            if(Logger.openLog) {
                Logger.e(TAG, "获取USB设备出错", e.getStackTrace());
            }
            e.printStackTrace();
        }
        return usbPaths;
    }

}
