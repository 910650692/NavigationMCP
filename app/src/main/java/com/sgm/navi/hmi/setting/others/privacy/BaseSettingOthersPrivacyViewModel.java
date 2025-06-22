package com.sgm.navi.hmi.setting.others.privacy;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseSettingOthersPrivacyViewModel extends BaseViewModel<SettingOthersPrivacyFragment, SettingOthersPrivacyModel> {

    public MutableLiveData<Boolean> mIsOneYearPrivacy = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mIsNeverPrivacy = new MutableLiveData<>(true);
    public MutableLiveData<String> mEndDate = new MutableLiveData<>("");


    public BaseSettingOthersPrivacyViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingOthersPrivacyModel initModel() {
        return new SettingOthersPrivacyModel();
    }

    /**
     * 根据获取的状态更新隐私协议界面显示.
     *
     * @param isTrue true表示一年，false永不
     * @param endDate 到期时间.
     */
    public void dualChoiceControl(final boolean isTrue, final String endDate) {
        mIsNeverPrivacy.setValue(!isTrue);
        mIsOneYearPrivacy.setValue(isTrue);
        mView.setEndDate(isTrue);
        mEndDate.setValue(endDate);
    }

    /**
     * 初始化各设置项状态值.
     */
    public void initView() {
        mModel.initView();
    }

    public Action mFinishPrivacy = () -> {
        closeFragment(true);
    };

    public Action mOneYearPrivacy = () -> {
        if (Boolean.TRUE.equals(mIsOneYearPrivacy.getValue())) {
            return;
        }
        mModel.setPrivacyStatus(true);
        sendBuryPointForSetPrivacyTime(BuryConstant.Number.ONE);
    };

    public Action mNeverPrivacy = () -> {
        if (Boolean.TRUE.equals(mIsNeverPrivacy.getValue())) {
            return;
        }
        mModel.setPrivacyStatus(false);
        sendBuryPointForSetPrivacyTime(BuryConstant.Number.SECOND);
    };

    @HookMethod(eventName = BuryConstant.EventName.AMAP_PRIVACY_SET)
    private void sendBuryPointForSetPrivacyTime(String isOneYearPrivacy){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, isOneYearPrivacy)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

}
