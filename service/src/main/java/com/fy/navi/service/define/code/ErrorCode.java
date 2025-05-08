package com.fy.navi.service.define.code;

import androidx.annotation.NonNull;

import java.util.HashMap;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;




/**
 * @author lvww
 * @date 2024/12/1
 */
@Setter
@Getter
public class ErrorCode {
    private Map<Integer, String> errorCode = new HashMap<>();
    private Map<Integer, String> engineCode = new HashMap<>();
    private Map<Integer, String> activateCode = new HashMap<>();
    private Map<Integer, String> activityCode = new HashMap<>();
    private Map<Integer, String> positionCode = new HashMap<>();
    private Map<Integer, String> mapCode = new HashMap<>();
    private Map<Integer, String> layerCode = new HashMap<>();
    private Map<Integer, String> searchCode = new HashMap<>();
    private Map<Integer, String> routeCode = new HashMap<>();
    private Map<Integer, String> naviCode = new HashMap<>();
    private Map<Integer, String> settingCode = new HashMap<>();
    private Map<Integer, String> mapDataCode = new HashMap<>();
    private Map<Integer, String> accountCode = new HashMap<>();
    private Map<Integer, String> forCastCode = new HashMap<>();


    @NonNull
    @Override
    public String toString() {
        return "ErrorCode{" +
                "errorCode=" + errorCode +
                ", engineCode=" + engineCode +
                ", activityCode=" + activityCode +
                ", positionCode=" + positionCode +
                ", mapCode=" + mapCode +
                ", layerCode=" + layerCode +
                ", searchCode=" + searchCode +
                ", routeCode=" + routeCode +
                ", naviCode=" + naviCode +
                ", settingCode=" + settingCode +
                ", mapDataCode=" + mapDataCode +
                ", accountCode=" + accountCode +
                ", forCastCode=" + forCastCode +
                '}';
    }
}
