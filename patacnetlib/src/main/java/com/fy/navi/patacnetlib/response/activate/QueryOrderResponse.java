package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;

import org.json.JSONException;

public class QueryOrderResponse extends BaseResponse {

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
            orderStatus = getMDataSet().getString("orderStatus");
        } catch (JSONException e) {
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
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
            activeCode = getMDataSet().getString("activeCode");
        } catch (JSONException e) {
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
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
            serialNumber = getMDataSet().getString("serialNumber");
        } catch (JSONException e) {
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
        }
        return serialNumber;
    }

    @NonNull
    @Override
    public String toString() {
        return "QueryOrderResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", orderStatus = '" + getOrderStatus() + '\'' +
                ", activeCode = '" + getActiveCode() + '\'' +
                ", serialNumber = '" + getSerialNumber() + '\'' +
                '}';
    }
}
