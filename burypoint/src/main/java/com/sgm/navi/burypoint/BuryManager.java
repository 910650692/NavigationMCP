package com.sgm.navi.burypoint;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import com.sgm.navi.burypoint.constant.BuryConstant;

import patac.manager.datatrack.PatacDataTrackManager;

public class BuryManager {

    private static final String TAG = "BuryManager";

    private static volatile BuryManager instance;

    private static final String APP_INIT_VERSION_NAME = "0.1";
    private static String mAppVersionName = APP_INIT_VERSION_NAME;
    private PatacDataTrackManager mPatacDataTrackManager = null;
    private Context context;
    private boolean isCar = false;

    public static BuryManager getInstance(){
        if (instance == null){
            synchronized (BuryManager.class){
                if (instance == null){
                    instance = new BuryManager();
                }
            }
        }
        return instance;
    }

    public void initPatacDataTrackManager(Context context, boolean isCar){
        this.isCar = isCar;
        if(!isCar) return;
        this.context = context;
        mPatacDataTrackManager = PatacDataTrackManager.getInstance(context);
    }

    public PatacDataTrackManager getPatacDataTrackManager() {
        return mPatacDataTrackManager;
    }

    public String getSid(){
        return BuryConstant.Model.S_ID;
    }

    public String getSvid(){
        return BuryConstant.Model.S_VID;
    }

    public String getAppId(){
        return BuryConstant.Model.APP_ID;
    }

    public boolean getCar() {
        return isCar;
    }

    public String getAppVersion() {
        if (!APP_INIT_VERSION_NAME.equals(mAppVersionName)) {
            // 初始化第一次的时候才获取应用版本号
            return mAppVersionName;
        }
        try {
            PackageInfo info = context.getPackageManager().getPackageInfo(context.getPackageName(), PackageManager.GET_CONFIGURATIONS);
            mAppVersionName = info.versionName;
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }
        return mAppVersionName;
    }

}
