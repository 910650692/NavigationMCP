package com.fy.navi.service.adapter.activate.cloudpatac.response;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.patac.netlib.bean.BaseResponse;

import org.json.JSONException;
import org.json.JSONObject;

public class CheckOrderResponse extends BaseResponse<JSONObject> {

    /**
     * 获取订单状态
     * 1 已下单/等待交付
     * 2 已完成
     * 3 已失败
     *
     * @return status
     */
    public String getOrderStatus() {
        String orderStatus = "";
        try {
            orderStatus = getDataSet().getString("orderStatus");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return orderStatus;
    }

    /**
     * 获取激活吗
     *
     * @return 激活码
     */
    public String getActiveCode() {
        String activeCode = "";
        try {
            activeCode = getDataSet().getString("activeCode");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return activeCode;
    }

    /**
     * 获取序列号
     *
     * @return 序列号
     */
    public String getSerialNumber() {
        String serialNumber = "";
        try {
            serialNumber = getDataSet().getString("serialNumber");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return serialNumber;
    }

    @NonNull
    @Override
    public String toString() {
        return "QueryOrderResponse{" +
                "resultCode='" + getResultCode() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", orderStatus = '" + getOrderStatus() + '\'' +
                ", activeCode = '" + getActiveCode() + '\'' +
                ", serialNumber = '" + getSerialNumber() + '\'' +
                '}';
    }
}
