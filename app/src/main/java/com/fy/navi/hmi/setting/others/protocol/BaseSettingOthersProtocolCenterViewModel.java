package com.fy.navi.hmi.setting.others.protocol;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseSettingOthersProtocolCenterViewModel extends BaseViewModel<SettingOthersProtocolCenterFragment, SettingOthersProtocolCenterModel> {
    public BaseSettingOthersProtocolCenterViewModel(@NonNull Application application) {
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

    public Action protocolCenterBack = () -> {
        closeFragment(true);
    };

    public Action openProtocolCenterTerm = () -> {
        mView.showProtocolCenter(SettingOthersProtocolCenterFragment.ProtocolCenterType.PROTOCOL_TERM);
    };

    public Action openProtocolCenterPrivacy = () -> {
        mView.showProtocolCenter(SettingOthersProtocolCenterFragment.ProtocolCenterType.PROTOCOL_PRIVACY);
    };

    public Action closeProtocolCenter = () -> {
        mView.closeProtocolCenter();
    };
}
