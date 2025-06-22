package com.sgm.navi.service.define.patacnet;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ResponseEntity<T> {
    @SerializedName("resultCode")  // 映射 BaseResponse 的 resultCode
    private String mResultCode;

    @SerializedName("message")
    private String mMessage;

    @SerializedName("dataSet")
    private T mDataSet;

    private boolean mIsSuccess;
}
