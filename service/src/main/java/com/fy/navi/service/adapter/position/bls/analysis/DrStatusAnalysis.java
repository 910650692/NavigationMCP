package com.fy.navi.service.adapter.position.bls.analysis;

import android.os.SystemClock;
import android.text.TextUtils;

import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.position.SensorCalibrationPara;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/***DR标定结果 分析(可做埋点或者本地存储) 预留***/
public class DrStatusAnalysis {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private String mPath = "";
    private static final String CONFIG_NAME = "config.txt";
    private SensorCalibrationPara mConfig = null;

    public DrStatusAnalysis() {
        long startTime = SystemClock.elapsedRealtime();
//        mPath = GBLCacheFilePath.POS_DIR + "pos" + "context/" + "hmi/";
//        FileUtils.getInstance().checkFileDir(mPath);
//        mConfig = readConfig();
        Logger.i(TAG, "init readConfig " + mConfig + " " +
                "path=" + mPath + "  cost mills=" + (SystemClock.elapsedRealtime() - startTime));
    }

    public void analysis(String dr) {
        if (TextUtils.isEmpty(dr)) {
            return;
        }
        if (mConfig == null) {
            mConfig = new SensorCalibrationPara(dr, System.currentTimeMillis(), -1);
            Logger.i(TAG, "analysis first, create config " + mConfig);
            asynWriteConfig(mConfig);
            return;
        }
        if (mConfig.getEndTime() > -1) {
            return;
        }

        if (parse(dr)) {
            mConfig.setEndTime(System.currentTimeMillis());
            mConfig.setPara(dr);
            asynWriteConfig(mConfig);
        } else {
            if (!dr.equals(mConfig.getPara())) {
                mConfig.setPara(dr);
                asynWriteConfig(mConfig);
            }
        }
    }

    private boolean parse(String data) {
        String[] arr = data.split("\\r?\\n");
        int validCount = 0;
        for (int i = 0; i < arr.length; i++) {
            if (TextUtils.isEmpty(arr[i])) {
                continue;
            }
            String t[] = arr[i].split(":");
            if (t.length < 2 || TextUtils.isEmpty(t[0]) || TextUtils.isEmpty(t[1])) {
                continue;
            }
            if (t[0].equals("车速补偿系数") || t[0].equals("陀螺仪Z轴零偏") || t[0].equals("安装角误差")) {
                if (!t[1].contains("不可用")) {
                    validCount++;
                } else {
                    Logger.i(TAG, "invalid ," + arr[i]);
                }
            } else if (t[0].trim().equals("安装角")) {
                if (t[1].trim().equals("有设置")) {
                    validCount++;
                } else {
                    Logger.i(TAG, "invalid ," + arr[i]);
                }
            }
        }
        Logger.i(TAG, "parse validCount=" + validCount);
        return validCount == 4;
    }

    private void asynWriteConfig(SensorCalibrationPara bean) {
//        TaskProxy.execute(() -> writeConfig(bean));
    }

    private void writeConfig(SensorCalibrationPara bean) {
        try {
            if (bean == null) {
                return;
            }
            Logger.i(TAG, "writeConfig：" + bean.toString());
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(mPath + CONFIG_NAME));
            out.writeObject(bean);
            out.close();
        } catch (Exception e) {
            Logger.e(TAG, "writeConfig Exception：" + e.toString());
        }
    }


    public SensorCalibrationPara readConfig() {
        if (TextUtils.isEmpty(mPath)) {
            return null;
        }
        try {
            File configFile = new File(mPath, CONFIG_NAME);
            if (!configFile.exists()) {
                Logger.i(TAG, "configFile is not exist");
                return null;
            }
            ObjectInputStream in = new ObjectInputStream(new FileInputStream(mPath + CONFIG_NAME));
            SensorCalibrationPara bean = (SensorCalibrationPara) in.readObject();
            in.close();
            return bean;
        } catch (Exception e) {
            Logger.e(TAG, "readConfig Exception：" + e.toString());
        }
        return null;
    }
}
