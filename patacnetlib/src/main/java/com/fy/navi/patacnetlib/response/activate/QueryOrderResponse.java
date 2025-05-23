package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;

@Getter
public class QueryOrderResponse extends BaseResponse {

    @SerializedName("orderStatus")
    private String mOrderStatus;
    @SerializedName("activeCode")
    private String mActiveCode;
    @SerializedName("serialNumber")
    private String mSerialNumber;

    @NonNull
    @Override
    public String toString() {
        return "QueryOrderResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", orderStatus = '" + getMOrderStatus() + '\'' +
                ", activeCode = '" + getMActiveCode() + '\'' +
                ", serialNumber = '" + getMSerialNumber() + '\'' +
                '}';
    }
}
