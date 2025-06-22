package com.sgm.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;

@Getter
public class CreateOrderResponse extends BaseResponse {
    @SerializedName("cusOrderId")
    private String mCusOrderId;

    @NonNull
    @Override
    public String toString() {
        return "CreateOrderResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", cusOrderId = '" + getMCusOrderId() + '\'' +
                '}';
    }

}
