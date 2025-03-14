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

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class CarConnectModel extends BaseModel<CarConnectViewModel> implements AccountCallBack, CarConnectCallBack {

    private static final String TAG = MapDefaultFinalTag.ACCOUNT_HMI_TAG;

    private final AccountPackage accountPackage;
    private final CarConnectPackage carConnectPackage;

    public CarConnectModel() {
        accountPackage = AccountPackage.getInstance();
        carConnectPackage = CarConnectPackage.getInstance();
        accountPackage.registerCallBack("CarConnectModel", this);
        carConnectPackage.registerCallBack(this);
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
    public void notifyAccountLogout(int errCode, int taskId, AccountUserInfo result) {
        Logger.i(TAG, "notifyAccountLogout: " + result);
        ThreadManager.getInstance().postUi(() -> {
            if (result != null && result.code == 1) {
                mViewModel.clearUserInfo();
                mViewModel.closeCurrentFragment();
            }
        });
    }
}
