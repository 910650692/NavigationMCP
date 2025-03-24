package com.fy.navi.hmi.carconnect;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.carconnect.CarConnectCallBack;
import com.fy.navi.service.logicpaket.user.carconnect.CarConnectPackage;
import com.fy.navi.ui.base.BaseModel;

public class CarConnectModel extends BaseModel<CarConnectViewModel> implements AccountCallBack, CarConnectCallBack {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    private final AccountPackage mAccountPackage;
    private final CarConnectPackage mCarConnectPackage;

    public CarConnectModel() {
        mAccountPackage = AccountPackage.getInstance();
        mCarConnectPackage = CarConnectPackage.getInstance();
        mAccountPackage.registerCallBack("CarConnectModel", this);
        mCarConnectPackage.registerCallBack(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {
        Logger.i(TAG, "notifyAccountLogout: " + result);
        ThreadManager.getInstance().postUi(() -> {
            if (result != null && result.code == 1) {
                mViewModel.clearUserInfo();
                mViewModel.closeCurrentFragment();
            }
        });
    }
}
