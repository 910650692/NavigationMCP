package com.sgm.navi.hmi.map;

import android.content.Context;
import android.content.Intent;

import com.android.utils.ConvertUtils;
import com.sgm.navi.mapservice.util.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.agreement.AgreementPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;

public class BroadcastManager implements SettingUpdateObservable.SettingUpdateObserver {
    private static final String TAG = BroadcastManager.class.getSimpleName();

    private static final String ACTION_SPI_COLLECTING_START = "patac.intent.ACTION_SPI_COLLECTING_START";
    private static final String ACTION_SPI_COLLECTING_STOP = "patac.intent.ACTION_SPI_COLLECTING_STOP";
    private static final String TARGET_PACKAGE = "com.android.systemui";
    private static final String PACKAGE_NAME = "com.sgm.navi.hmi";
    private static final String CLASS_NAME = "com.sgm.navi.hmi.map.MapActivity";
    private static final String TYPE = "LOCATION";

    private boolean isBroadcasting = false;

    private BroadcastManager() {

    }

    public void init() {
        SettingUpdateObservable.getInstance().addObserver(TAG, this);
    }

    public void unInit() {
        SettingUpdateObservable.getInstance().removeObserver(TAG, this);
    }

    public void sendSpiCollectingBroadcast() {
        try {
            if (AppCache.getInstance().getMContext() == null) {
                Logger.e(TAG, "Context is null, cannot send broadcast");
                return;
            }
            Logger.d(TAG, "sendSpiCollectingBroadcast: isBroadcasting = ", isBroadcasting);
            if (isBroadcasting) {
                Logger.d(TAG, "已经在发送广播，忽略重复请求");
                return;
            }
            final boolean isAutoAgreed = AgreementPackage.getInstance().isAllowAutoAgreement();
            final boolean isSGMAgreed = AgreementPackage.getInstance().isAllowSGMAgreement();
            Logger.d(TAG, "sendBroadcast: isAutoAgreed = ", isAutoAgreed,
                    "PrivacyStatus = ", SettingPackage.getInstance().getPrivacyStatus(),
                    "isSGMAgreed = ", isSGMAgreed);
            if (!(isAutoAgreed && SettingPackage.getInstance().getPrivacyStatus() && isSGMAgreed)) {
                Logger.e(TAG, "有权限未满足，不发送广播");
                return;
            }

            Intent intent = new Intent();
            intent.setAction(ACTION_SPI_COLLECTING_START);
            intent.setPackage(TARGET_PACKAGE);
            intent.putExtra("packageName", PACKAGE_NAME);
            intent.putExtra("className", CLASS_NAME);
            intent.putExtra("type", TYPE);

            AppCache.getInstance().getMContext().sendBroadcast(intent);
            isBroadcasting = true;
        } catch (Exception e) {
            Logger.e(TAG, "发送SPI收集广播失败", e);
        }
    }

    public void sendSpiCollectingStopBroadcast() {
        try {
            if (AppCache.getInstance().getMContext() == null) {
                Logger.e(TAG, "Context is null, cannot send stop broadcast");
                return;
            }
            Logger.d(TAG, "sendSpiCollectingStopBroadcast: isBroadcasting = ", isBroadcasting);
            if (!isBroadcasting) {
                Logger.d(TAG, "没有正在发送的广播，忽略停止请求");
                return;
            }

            Intent intent = new Intent();
            intent.setAction(ACTION_SPI_COLLECTING_STOP);
            intent.setPackage(TARGET_PACKAGE);

            intent.putExtra("packageName", PACKAGE_NAME);
            intent.putExtra("className", CLASS_NAME);
            intent.putExtra("type", TYPE);

            AppCache.getInstance().getMContext().sendBroadcast(intent);
            isBroadcasting = false;
        } catch (Exception e) {
            Logger.e(TAG, "发送SPI收集停止广播失败", e);
        }
    }

    @Override
    public void onUpdateSetting(String key, boolean value) {
        if (SettingController.KEY_SETTING_PRIVACY_STATUS.equals(key)) {
            if (!value) {
                Logger.e(TAG, "隐私被拒绝，发送退出广播");
                sendSpiCollectingStopBroadcast();
            } else {
                Logger.e(TAG, "隐私被同意，发送开始广播");
                sendSpiCollectingBroadcast();
            }
        }
    }

    private static final class InstanceHolder {
        static final BroadcastManager INSTANCE = new BroadcastManager();
    }

    public static BroadcastManager getInstance() {
        return InstanceHolder.INSTANCE;
    }

}
