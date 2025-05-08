package com.fy.navi.service.adapter.search.cloudByPatac.bean;

public class SavedStations {
    private String operatorId;
    private String stationId;
    private Boolean stationSaved; // 充电站是否收藏
    private Long lastUpdateTime; // 收藏/取消收藏操作时的时间戳
}
