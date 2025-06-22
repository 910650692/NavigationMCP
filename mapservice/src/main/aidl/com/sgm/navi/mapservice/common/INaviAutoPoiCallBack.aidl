package com.sgm.navi.mapservice.common;

interface INaviAutoPoiCallBack {

    void onChargingStationInform(String chargingStationInfo);

    void onParkingLotInform(String parkingLotInfo);

    void onSAPAInform(String serviceInfo);

    void onGasStationInform(String gasStationInfo);
}