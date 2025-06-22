package com.sgm.navi.mapservice.callback;

public interface OnPoiInformListener {

    /**
     * 充电站通知
     *
     * @param chargingStationInfo String
     */
    void onChargingStationInform(String chargingStationInfo);

    /**
     * 停车场通知
     *
     * @param parkingLotInfo String
     */
    void onParkingLotInform(String parkingLotInfo);

    /**
     * 服务区通知
     *
     * @param serviceInfo String
     */
    void onSAPAInform(String serviceInfo);

    /**
     * 加油站通知
     *
     * @param gasStationInfo String
     */
    void onGasStationInform(String gasStationInfo);
}
