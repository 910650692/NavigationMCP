package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;

@Getter
public class UuidResponse extends BaseResponse {
    @SerializedName("vin")
    private String mVin;

    @NonNull
    @Override
    public String toString() {
        return "UuidResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", uuid = '" + getMVin() + '\'' +
                '}';
    }
}
