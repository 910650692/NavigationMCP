package com.fy.navi.hmi.account;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAccountPhoneNumberLoginViewModel extends BaseViewModel<AccountPhoneNumberLoginFragment, AccountPhoneNumberLoginModel> {

    private String mPhoneNumber;
    private String mVerificationCode;

    public BaseAccountPhoneNumberLoginViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected AccountPhoneNumberLoginModel initModel() {
        return new AccountPhoneNumberLoginModel();
    }

    public void setPhoneNumber(final String phoneNumber) {
        this.mPhoneNumber = phoneNumber;
    }

    public void setVerificationCode(final String verificationCode) {
        this.mVerificationCode = verificationCode;
    }

    //返回上一页
    public Action mAccountBack = () -> {
        closeFragment(true);
    };


    public Action mGetVerificationCode = () -> {
        AccountPackage.getInstance().verificationCodeRequest(mPhoneNumber);
    };

    public Action mClickLogin = () -> {
        AccountPackage.getInstance().mobileLoginRequest(mVerificationCode, mPhoneNumber);
    };

    public Action mClickQRCodeLogin = () -> {
        addFragment(new AccountQRCodeLoginFragment(), null);
    };

}
