package com.fy.navi.service.define.code;

import androidx.annotation.NonNull;

import java.util.HashMap;
import java.util.Map;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/1
 */
public class ErrorCode {
    private Map<Integer, String> errorCode = new HashMap<>();
    private Map<Integer, String> engineCode = new HashMap<>();
    private Map<Integer, String> layerCode = new HashMap<>();
    private Map<Integer, String> mapCode = new HashMap<>();
    private Map<Integer, String> searchCode = new HashMap<>();
    private Map<Integer, String> routeCode = new HashMap<>();
    private Map<Integer, String> naviCode = new HashMap<>();
    private Map<Integer, String> settingCode = new HashMap<>();
    private Map<Integer, String> mapDataCode = new HashMap<>();
    private Map<Integer, String> accountCode = new HashMap<>();

    public Map<Integer, String> getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(Map<Integer, String> errorCode) {
        this.errorCode = errorCode;
    }

    public Map<Integer, String> getEngineCode() {
        return engineCode;
    }

    public void setEngineCode(Map<Integer, String> engineCode) {
        this.engineCode = engineCode;
    }

    public Map<Integer, String> getLayerCode() {
        return layerCode;
    }

    public void setLayerCode(Map<Integer, String> layerCode) {
        this.layerCode = layerCode;
    }

    public Map<Integer, String> getMapCode() {
        return mapCode;
    }

    public void setMapCode(Map<Integer, String> mapCode) {
        this.mapCode = mapCode;
    }

    public Map<Integer, String> getSearchCode() {
        return searchCode;
    }

    public void setSearchCode(Map<Integer, String> searchCode) {
        this.searchCode = searchCode;
    }

    public Map<Integer, String> getRouteCode() {
        return routeCode;
    }

    public void setRouteCode(Map<Integer, String> routeCode) {
        this.routeCode = routeCode;
    }

    public Map<Integer, String> getNaviCode() {
        return naviCode;
    }

    public void setNaviCode(Map<Integer, String> naviCode) {
        this.naviCode = naviCode;
    }

    public Map<Integer, String> getSettingCode() {
        return settingCode;
    }

    public void setSettingCode(Map<Integer, String> settingCode) {
        this.settingCode = settingCode;
    }

    public Map<Integer, String> getMapDataCode() {
        return mapDataCode;
    }

    public void setMapDataCode(Map<Integer, String> mapDataCode) {
        this.mapDataCode = mapDataCode;
    }

    public Map<Integer, String> getAccountCode() {
        return accountCode;
    }

    public void setAccountCode(Map<Integer, String> accountCode) {
        this.accountCode = accountCode;
    }

    @NonNull
    @Override
    public String toString() {
        return "ErrorCode{" +
                "engineCode=" + engineCode +
                ", mapCode=" + mapCode +
                ", searchCode=" + searchCode +
                ", routeCode=" + routeCode +
                ", naviCode=" + naviCode +
                ", settingCode=" + settingCode +
                ", mapDataCode=" + mapDataCode +
                ", accountCode=" + accountCode +
                '}';
    }
}
