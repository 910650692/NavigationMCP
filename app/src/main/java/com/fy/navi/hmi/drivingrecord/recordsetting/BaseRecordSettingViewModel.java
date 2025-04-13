package com.fy.navi.hmi.drivingrecord.recordsetting;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class BaseRecordSettingViewModel extends BaseViewModel<RecordSettingFragment, RecordSettingModel> {

    public BaseRecordSettingViewModel(@NonNull final Application application) {
        super(application);
    }

    public MutableLiveData<Boolean> mIsAutoRecord = new MutableLiveData<>(true);

    @Override
    protected RecordSettingModel initModel() {
        return new RecordSettingModel();
    }

    /**
     * 初始化View
     */
    public void initView() {
        mModel.initView();
    }

    public Action mCloseRecordSetting = () -> {
       closeFragment(true);
    };

    public Action mClearDivingRecord = () -> {
        mView.clearDivingRecord();
    };

    public Action mSwitchRecordSetting = () -> {
        final boolean value = Boolean.FALSE.equals(mIsAutoRecord.getValue());
        mIsAutoRecord.setValue(value);
        mModel.setAutoRecord(value);
    };

    /**
     * 设置是否自动记录
     * @param isAutoRecord 是否自动记录
     */
    public void setIsAutoRecord(final boolean isAutoRecord) {
        this.mIsAutoRecord.setValue(isAutoRecord);
    }

    /**
     * 设置清除按钮是否可用
     * @param isEnable 是否可用
     */
    public void setClearButtonEnable(final boolean isEnable) {
        mView.setClearButtonEnable(isEnable);
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        return mModel.getDrivingRecordDataFromSdk();
    }
    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     */
    public void delBehaviorData(final String id) {
        mModel.delBehaviorData(id);
    }

    /**
     * 通过数据type删除其对应info
     * @param type 数据type
     */
    public void deleteValueByKey(final int type) {
        mModel.deleteValueByKey(type);
    }
}
