package com.fy.navi.hmi.drivingrecord.recordsetting;

import com.fy.navi.hmi.drivingrecord.DrivingRecordViewModel;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.base.BaseModel;

public class RecordSettingModel extends BaseModel<RecordSettingViewModel> {

    private SettingManager settingManager;

    public RecordSettingModel() {
        settingManager = SettingManager.getInstance();
        settingManager.init();
    }

    public void initView() {
        getAutoRecord();
    }

    public void setAutoRecord(boolean isAutoRecord) {
        settingManager.insertOrReplace(SettingController.KEY_SETTING_IS_AUTO_RECORD, String.valueOf(isAutoRecord));
    }

    public void getAutoRecord() {
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_IS_AUTO_RECORD);
        mViewModel.setIsAutoRecord(Boolean.parseBoolean(value));
    }
}
