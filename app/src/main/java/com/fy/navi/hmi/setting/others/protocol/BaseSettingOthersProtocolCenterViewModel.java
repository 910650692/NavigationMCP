package com.fy.navi.hmi.setting.others.protocol;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersProtocolCenterViewModel extends BaseViewModel<SettingOthersProtocolCenterFragment, SettingOthersProtocolCenterModel> {
    public BaseSettingOthersProtocolCenterViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SettingOthersProtocolCenterModel initModel() {
        return new SettingOthersProtocolCenterModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action mProtocolCenterBack = () -> {
        closeFragment(true);
    };

    public Action mOpenProtocolCenterTerm = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_SERVICEAGREEMENT_CHECK)
        public void call() {
            mView.showProtocolCenter(SettingOthersProtocolCenterFragment.ProtocolCenterType.PROTOCOL_TERM);
        }
    };

    public Action mOpenProtocolCenterPrivacy = () -> {
        mView.showProtocolCenter(SettingOthersProtocolCenterFragment.ProtocolCenterType.PROTOCOL_PRIVACY);
    };

    public Action mCloseProtocolCenter = () -> {
        mView.closeProtocolCenter();
    };
}
