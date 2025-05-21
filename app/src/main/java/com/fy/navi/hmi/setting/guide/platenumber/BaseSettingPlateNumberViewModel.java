package com.fy.navi.hmi.setting.guide.platenumber;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.Objects;

public class BaseSettingPlateNumberViewModel extends BaseViewModel<SettingPlateNumberFragment, SettingPlateNumberModel> {

    private static final String TAG = MapDefaultFinalTag.SETTING_HMI_TAG;
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

    /**
     * 更新车牌号
     * @param plateNumber 车牌号
     */
    public void onPlateNumberChanged(final String plateNumber) {
        Logger.d(TAG, "onPlateNumberChanged: plateNumber");
        final boolean isPlateNumberEmpty = Objects.equals(plateNumber, "");
        if (!isPlateNumberEmpty)  {
            if (mModel.setConfigKeyAvoidLimit(true) == 0) {
                mModel.setConfigKeyPlateNumber(plateNumber);
            }
        } else {
            if (mModel.setConfigKeyAvoidLimit(false) == 0) {
                mModel.setConfigKeyPlateNumber("");
            }
        }
    }
}
