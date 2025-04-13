package com.fy.navi.hmi.drivingrecord.recordsetting;

import android.text.TextUtils;

import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

public class RecordSettingModel extends BaseModel<RecordSettingViewModel> {

    private final SettingManager mSettingManager;
    private final UserTrackPackage mUserTrackPackage;
    private final HistoryManager mHistoryManager;

    public RecordSettingModel() {
        mSettingManager = SettingManager.getInstance();
        mSettingManager.init();
        mUserTrackPackage = UserTrackPackage.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    /**
     * 初始化View
     */
    public void initView() {
        getAutoRecord();
        if (getBehaviorDataIds() != null) {
            mViewModel.setClearButtonEnable(getBehaviorDataIds().length > 0);
        } else {
            mViewModel.setClearButtonEnable(false);
        }
    }

    /**
     * 设置是否自动记录
     * @param isAutoRecord 是否自动记录
     */
    public void setAutoRecord(final boolean isAutoRecord) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_IS_AUTO_RECORD, String.valueOf(isAutoRecord));
    }

    /**
     * 获取是否自动记录
     */
    public void getAutoRecord() {
        String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_IS_AUTO_RECORD);
        if (value == null || TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
            setAutoRecord(true);
        }
        mViewModel.setIsAutoRecord(Boolean.parseBoolean(value));
    }

    /**
     * 获取行为数据id列表
     * @return 行为数据id列表
     */
    public int[] getBehaviorDataIds() {
        return mUserTrackPackage.getBehaviorDataIds();
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        return mUserTrackPackage.getDrivingRecordDataFromSdk();
    }

    /**
     * 删除行为数据
     * @param id 行为数据id
     */
    public void delBehaviorData(final String id) {
        mUserTrackPackage.delBehaviorData(id);
    }

    /**
     * 通过数据type删除其对应info
     * @param type 数据type
     */
    public void deleteValueByKey(final int type) {
        mHistoryManager.deleteValueByKey(type);
    }
}
