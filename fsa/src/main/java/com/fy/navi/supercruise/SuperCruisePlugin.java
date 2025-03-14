package com.fy.navi.supercruise;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

import gm.navigation.GMNavigationManager;
import gm.navigation.NavigationException;
import gm.navigation.NavigationPluginBase;
import gm.navigation.enums.NavigationAction;
import gm.navigation.enums.NavigationAppType;
import gm.navigation.models.NavigationAppInfo;
import gm.navigation.models.NavilinkData;

public class SuperCruisePlugin extends NavigationPluginBase {
    private final static String TAG = SuperCruisePlugin.class.getSimpleName();

    private String NAME = "BkNaviApp"; // TODO 通过接口获取
    private String VERSION = "1.0"; // TODO 通过接口获取
    private String PACKAGE_NAME = "com.fy.navi.hmi";
    private NavigationAppType TYPE = NavigationAppType.THIRD_PARTY;
    private int CAPABILITIES = NavigationAppInfo.CAPABILITY_REPORT_ROAD_INFO;
    private GMNavigationManager mGMNavigationManager;

    public SuperCruisePlugin(Context context) {
        super(context);
        mGMNavigationManager = new GMNavigationManager(context);
    }

    public void init() {
        try {
            mGMNavigationManager.registerNavigationPlugin(this);
            Log.d(TAG, "Register NaviPlugin");
        } catch (NavigationException ne) {
            Log.e(TAG, "Cannot register NaviPlugin", ne);
        }
    }

    public void deinit() {
        try {
            mGMNavigationManager.unregisterNavigationPlugin(this);
            Log.d(TAG, "Unregister NaviPlugin");
        } catch (NavigationException ne) {
            Log.e(TAG, "Cannot unregister NaviPlugin", ne);
        }
    }

    public void pluginNotifyFocusGranted() {
        Log.i(TAG, "pluginNotifyFocusGranted");
        notifyFocusGranted();
    }

    public void pluginNotifyFocusLost() {
        Log.i(TAG, "pluginNotifyFocusLost");
        notifyFocusLost();
    }

    public void sendData(NavilinkData.NavilinkDataMemeber data) {
        pluginNotifyFocusGranted();
        NavilinkData navilinkData = new NavilinkData();
        navilinkData.addDataMember(data);
        reportNavilinkData(navilinkData);
    }

    @Override
    protected NavigationAppInfo onGetNavigationAppInfo() {
        NavigationAppInfo info = new NavigationAppInfo();
        info.setName(NAME);
        info.setVersion(VERSION);
        info.setPackageName(PACKAGE_NAME);
        info.setAppType(TYPE);
        info.setCapabilities(CAPABILITIES);
        return info;
    }

    @Override
    protected Intent onGetNavigationIntent() {
        return null;
    }

    @Override
    protected Intent onGetCardViewIntent() {
        return null;
    }

    @Override
    protected Intent onGetNavigationSessionIntent(Bundle bundle) {
        return null;
    }

    @Override
    protected Intent onGetNavigationSearchIntent(Bundle bundle) {
        return null;
    }

    @Override
    protected boolean onSendNavigationAction(NavigationAction navigationAction) {
        return false;
    }

    @Override
    protected boolean onStartMapFeed() {
        return false;
    }

    @Override
    protected boolean onStopMapFeed() {
        return false;
    }
}
