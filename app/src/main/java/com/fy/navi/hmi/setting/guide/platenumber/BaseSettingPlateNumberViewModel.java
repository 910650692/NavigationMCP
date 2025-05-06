package com.fy.navi.hmi.setting.guide.platenumber;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingPlateNumberViewModel extends BaseViewModel<SettingPlateNumberFragment, SettingPlateNumberModel> {

    public MutableLiveData<Boolean> mIsFocus = new MutableLiveData<>(false);

    private MutableLiveData<Boolean> mIsDeletePlateNumberDialog= new MutableLiveData<>(false);

    private String mPlateNumber = "";

    private String mProvince = "";

    public BaseSettingPlateNumberViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingPlateNumberModel initModel() {
        return new SettingPlateNumberModel();
    }

    public Action mShowProvinceKeyboard = () -> {
        mView.showProvinceKeyboard();
        mIsFocus.setValue(true);
    };

    public Action mHideKeyboard = () -> {
        mView.hideKeyboard();
    };

    public Action mClosePlateNumber = () -> {
        closeFragment(true);
    };

    public Action mShowPlateNumberKeyboard = () -> {
        mView.showPlateNumberKeyboard();
    };
     public Action mPlateNumberInputFinish = () -> {
        mView.plateNumberInputFinish();
    };

     public Action mClearPlateNumber = () -> {
        mView.clearPlateNumber();
    };

    /**
     * 获取车牌
     * @return 车牌
     */
     public String getPlateNumber() {
        return mModel.getPlateNumber();
    }

    /**
     * 设置车牌号
     * @param plateNumber 车牌号
     */
    public void setPlateNumber(final String plateNumber) {
        mModel.setPlateNumber(plateNumber);
    }

    /**
     * 设置是否限行
     * @param avoidLimit 是否限行
     */
    public void setAvoidLimit(final boolean avoidLimit) {
        mModel.setAvoidLimit(avoidLimit);
    }

    /**
     * @param isFocus 是否获取焦点
     */
    public void setIsFocus(final boolean isFocus) {
        this.mIsFocus.setValue(isFocus);
    }

    /**
     * 是否显示删除车牌的对话框
     * @param isDeletePlateNumberDialog 对话框
     */
    public void setIsDeletePlateNumberDialog(final boolean isDeletePlateNumberDialog){
        mIsDeletePlateNumberDialog.setValue(isDeletePlateNumberDialog);
    }

    public boolean getIsDeletePlateNumberDialog(){
        return Boolean.TRUE.equals(mIsDeletePlateNumberDialog.getValue());
    }

    public String getPlateNumberString() {
        return mPlateNumber;
    }

    public void setPlateNumberString(final String plateNumberString) {
        mPlateNumber = plateNumberString;
    }

    public String getProvince() {
        return mProvince;
    }

    public void setProvince(final String province) {
        mProvince = province;
    }
}
