package com.fy.navi.hmi.setting.others.about;

import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.hotupdate.MapNumInfo;
import com.fy.navi.service.logicpaket.hotupdate.HotUpdateCallback;
import com.fy.navi.service.logicpaket.hotupdate.HotUpdatePackage;
import com.fy.navi.ui.base.BaseModel;

public class SettingOthersAboutModel extends BaseModel<SettingOthersAboutViewModel> implements HotUpdateCallback {

    private final SettingManager mSettingManager;
    private final HotUpdatePackage mHotUpdatePackage;

    public SettingOthersAboutModel() {
        mSettingManager = SettingManager.getInstance();
        mSettingManager.init();
        mHotUpdatePackage = HotUpdatePackage.getInstance();
    }

    /**
     * 初始化view
     */
    public void initView() {
        getChannelID();
        getMapDataID();
        getSdkVersion();
    }


    @Override
    public void onCreate() {
        super.onCreate();
        mHotUpdatePackage.registerCallBack("SettingOthersAboutModel",this);
        updateVersion();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }


    /**
     * 设置渠道号
     */
    private void getChannelID() {
        final String channelID = mSettingManager.getValueByKey(SettingController.KEY_SETTING_CHANNEL_ID);
        mViewModel.setChannelID(channelID);
    }

    /**
     * 设置数据版本号
     */
    public void getMapDataID() {
        final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        final int adCode = MapDataPackage.getInstance().getAdCodeByLonLat(locInfoBean.getLongitude(), locInfoBean.getLatitude());
        final String mapDataID = MapDataPackage.getInstance().getDataFileVersion(adCode);
        mViewModel.setMapDataVersion(mapDataID);
    }

    /**
     * 设置sdk版本号
     */
    public void getSdkVersion() {
        final String sdkVersion = EnginePackage.getInstance().getSdkVersion();
        mViewModel.setSdkVersion(sdkVersion);
    }

    /**
     * 获取互联网审图号
     */
    public void updateVersion() {
        final MapNumInfo mapNumInfo = new MapNumInfo();
        mapNumInfo.setStrKey("internet");
        HotUpdatePackage.getInstance().requestMapNum(mapNumInfo);
    }

    @Override
    public void onRequestMapNum(final int errorCode, final MapNumInfo mapNumInfo) {
        Logger.d("onRequestMapNum: " + mapNumInfo.getStrKey() + " " + mapNumInfo.getStrVersion() + " " + mapNumInfo.getStrContent());
    }
}
