package com.fy.navi.hmi.account;

import android.app.Application;
import android.os.Handler;
import android.os.Looper;

import androidx.annotation.NonNull;

import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAccountPhoneNumberLoginViewModel extends BaseViewModel<AccountPhoneNumberLoginFragment, AccountPhoneNumberLoginModel> {

    private String phoneNumber;
    private String verificationCode;

    public BaseAccountPhoneNumberLoginViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected AccountPhoneNumberLoginModel initModel() {
        return new AccountPhoneNumberLoginModel();
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public void setVerificationCode(String verificationCode) {
        this.verificationCode = verificationCode;
    }

    //返回上一页
    public Action accountBack = () -> {
        closeFragment(true);
    };


    public Action getVerificationCode = () -> {
        AccountPackage.getInstance().verificationCodeRequest(phoneNumber);
    };

    public Action clickLogin = () -> {
        AccountPackage.getInstance().mobileLoginRequest(verificationCode, phoneNumber);
    };

    public Action clickQRCodeLogin = () -> {
        addFragment(new AccountQRCodeLoginFragment(), null);
    };

    public void close() {
        new Handler(Looper.getMainLooper()).post(() -> {
            closeFragment(true);
        });
    }
}
