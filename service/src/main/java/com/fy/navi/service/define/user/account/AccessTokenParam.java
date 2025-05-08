package com.fy.navi.service.define.user.account;

import android.accounts.AccountManagerCallback;
import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;

import java.util.Arrays;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class AccessTokenParam {

    //账户类型
    //ACCOUNT_TYPE_PATAC_HMI = "com.patac.hmi.account"
    private String mAccountType;

    //token类型
    //AUTH_TOKEN_TYPE_READ_ONLY = "read_only";
    //AUTH_TOKEN_TYPE_FULL_ACCESS = "full_access";
    private String mAuthTokenType;

    //可以为null
    private String[] mFeatures;

    //context
    private Activity mActivity;

    //可以为null
    private Bundle mAddAccountOption;

    @Override
    public String toString() {
        return "AccessTokenParam{" +
                "mAccountType='" + mAccountType + '\'' +
                ", mAuthTokenType='" + mAuthTokenType + '\'' +
                ", mFeatures=" + Arrays.toString(mFeatures) +
                ", mActivity=" + mActivity +
                ", mAddAccountOption=" + mAddAccountOption +
                ", mGetAuthTokenOption=" + mGetAuthTokenOption +
                ", mCallback=" + mCallback +
                ", mHandler=" + mHandler +
                '}';
    }

    //可以为null
    private Bundle mGetAuthTokenOption;

    //可以为null
    private AccountManagerCallback<Bundle> mCallback;

    //表示callBack thread,可以为null
    private Handler mHandler;
}
