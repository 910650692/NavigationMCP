package com.fy.navi.hmi.account;

import android.app.Application;
import android.graphics.Bitmap;
import android.os.Handler;
import android.os.Looper;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseAccountQRCodeLoginViewModel extends BaseViewModel<AccountQRCodeLoginFragment, AccountQRCodeLoginModel> {

    public BaseAccountQRCodeLoginViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected AccountQRCodeLoginModel initModel() {
        return new AccountQRCodeLoginModel();
    }

    public Action accountBack = () -> {
        closeFragment(true);
    };

    public Action clickPhoneNumberLogin = () -> {
        addFragment(new AccountPhoneNumberLoginFragment(), null);
    };

    public void updateQRCode(Bitmap bitmap) {
        mView.updateQRCode(bitmap);
    }

    public void close() {
        new Handler(Looper.getMainLooper()).post(() -> {
            closeFragment(true);
        });
    }

    public void qRCodeLoginRequest(int qrType){
        mModel.qRCodeLoginRequest(qrType);
    }
}
