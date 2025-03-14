package com.fy.navi.hmi.launcher;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/17
 * Description: [Launcher 管理类]
 * TODO 待测试和联调
 */
public class LauncherManager {
    private static final String TAG = "LauncherManager";

    private LauncherManager() {

    }

    private static final class Holder {
        private static final LauncherManager instance = new LauncherManager();
    }

    public static LauncherManager getInstance() {
        return Holder.instance;
    }

    public void startInitService() {
        Logger.i(TAG, "startInitEngine");
        Intent intent = new Intent(AppContext.mContext, NaviService.class);
        ActivityCompat.startForegroundService(AppContext.mContext, intent);
    }

    /***
     * 启动Navi_App
     */
    public void startMapActivity(int pageCode) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppContext.mContext, MapActivity.class);
        intent.putExtra(INaviConstant.PAGE_EXTRA, pageCode);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        AppContext.mContext.startActivity(intent);
    }

    /***
     * 启动Navi_App
     */
    public void startMapActivity(int pageCode, @Nullable PoiInfoEntity poiInfo) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppContext.mContext, MapActivity.class);
        Bundle bundle = new Bundle();
        bundle.putInt(INaviConstant.PAGE_EXTRA, pageCode);
        if (poiInfo != null) {
            bundle.putParcelable(INaviConstant.POI_INFO_EXTRA, poiInfo);
        }
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.putExtras(bundle);
        AppContext.mContext.startActivity(intent);
    }
}
