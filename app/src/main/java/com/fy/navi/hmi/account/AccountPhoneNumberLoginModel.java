package com.fy.navi.hmi.account;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.BaseModel;

public class AccountPhoneNumberLoginModel extends BaseModel<AccountPhoneNumberLoginViewModel> implements AccountCallBack {

    private final AccountPackage accountPackage;

    public AccountPhoneNumberLoginModel() {
        accountPackage = AccountPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        accountPackage.registerCallBack("AccountPhoneNumberLoginModel",this);
    }

    @Override
    public void notifyMobileLogin(int errCode, int taskId, AccountUserInfo result) {
        if (errCode == 0 && result.code == 1) {
            Logger.i("AccountLoginModel AccountUserInfo = " + GsonUtils.toJson(result));
            Logger.i("AccountLoginModel", "手机号登录成功");
            mViewModel.close();
        }
    }
}
