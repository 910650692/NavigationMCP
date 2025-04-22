package com.fy.navi.hmi.drivingrecord.recordsetting;

import android.text.TextUtils;

import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;

public class RecordSettingModel extends BaseModel<RecordSettingViewModel> {

    private final SettingManager mSettingManager;
    private final UserTrackPackage mUserTrackPackage;
    private final HistoryManager mHistoryManager;
    private final AccountPackage mAccountPackage;

    public RecordSettingModel() {
        mSettingManager = SettingManager.getInstance();
        mSettingManager.init();
        mUserTrackPackage = UserTrackPackage.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
        mAccountPackage = AccountPackage.getInstance();
    }

    /**
     * 初始化View
     */
    public void initView() {
        getAutoRecord();
        if (!mAccountPackage.isLogin()) {
            final List<History> historyList = mHistoryManager.getValueByType(2);
            mViewModel.setClearButtonEnable(historyList != null && !historyList.isEmpty());
        } else {
            final int[] dataIds = mUserTrackPackage.getBehaviorDataIds();
            mViewModel.setClearButtonEnable(dataIds != null && dataIds.length > 0);
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
