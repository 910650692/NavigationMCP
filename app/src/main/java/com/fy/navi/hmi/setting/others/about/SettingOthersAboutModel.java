package com.fy.navi.hmi.setting.others.about;

import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.ui.base.BaseModel;

public class SettingOthersAboutModel extends BaseModel<SettingOthersAboutViewModel> {

    private final SettingManager settingManager;

    public SettingOthersAboutModel() {
        settingManager = SettingManager.getInstance();
        settingManager.init();
    }

    public void initView() {
        getChannelID();
        getMapDataID();
        getSdkVersion();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    private void getChannelID() {
        String channelID = settingManager.getValueByKey(SettingController.KEY_SETTING_CHANNEL_ID);
        mViewModel.setChannelID(channelID);
    }

    public void getMapDataID() {
        LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        int adCode = MapDataPackage.getInstance().getAdCodeByLonLat(locInfoBean.getLongitude(), locInfoBean.getLatitude());
        String mapDataID = MapDataPackage.getInstance().getDataFileVersion(adCode);
        mViewModel.setMapDataVersion(mapDataID);
    }

    public void getSdkVersion(){
        String sdkVersion = EnginePackage.getInstance().getSdkVersion();
        mViewModel.setSdkVersion(sdkVersion);
    }
}
