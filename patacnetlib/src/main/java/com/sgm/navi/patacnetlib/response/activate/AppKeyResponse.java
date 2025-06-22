package com.sgm.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;


@Getter
public class AppKeyResponse extends BaseResponse {
    @SerializedName("appKey")
    private String mAppKey;
    @SerializedName("expirationMinutes")
    private int mExpirationMinutes;

    @NonNull
    @Override
    public String toString() {
        return "AppKeyResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", appKey = '" + getMAppKey() + '\'' +
                ", expirationMinutes = '" + getMExpirationMinutes() + '\'' +
                '}';
    }

}
