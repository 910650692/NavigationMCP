package com.fy.navi.hmi.drivingrecord.recordsetting;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseRecordSettingViewModel extends BaseViewModel<RecordSettingFragment, RecordSettingModel> {

    public BaseRecordSettingViewModel(@NonNull Application application) {
        super(application);
    }

    public MutableLiveData<Boolean> isAutoRecord = new MutableLiveData<>(true);

    @Override
    protected RecordSettingModel initModel() {
        return new RecordSettingModel();
    }

    public void initView() {
        mModel.initView();
    }

    public Action closeRecordSetting = () -> {
       closeFragment(true);
    };

    public Action clearDivingRecord = () -> {
        mView.clearDivingRecord();
    };

    public Action switchRecordSetting = () -> {
        boolean value = Boolean.FALSE.equals(isAutoRecord.getValue());
        isAutoRecord.setValue(value);
        mModel.setAutoRecord(value);
    };

    public void setIsAutoRecord(boolean isAutoRecord) {
        this.isAutoRecord.setValue(isAutoRecord);
    }
}
