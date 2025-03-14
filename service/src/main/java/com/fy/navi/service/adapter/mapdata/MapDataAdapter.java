package com.fy.navi.service.adapter.mapdata;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.Objects;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(MapDataAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "MapDataAdapterImpl";
    private IMapDataApi mMapDataApi;

    private MapDataAdapter() {
        mMapDataApi = (IMapDataApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initMapDataService() {
        mMapDataApi.init();
    }

    public String getDataFileVersion(int adCode) {
        return mMapDataApi.getDataFileVersion(adCode);
    }

    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataApi.getMapDataList();
    }

    public CityDataInfo getCountryData() {
        return mMapDataApi.getCountryData();
    }

    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataApi.getAllDownLoadedList();
    }

    public ArrayList<ProvDataInfo> searchAdCode(String strKey) {
        return mMapDataApi.searchAdCode(strKey);
    }

    public ArrayList<ProvDataInfo> searchDownLoaded(String strKey) {
        return mMapDataApi.searchDownLoaded(strKey);
    }

    public int searchCityAdCode(String strKey) {
        return mMapDataApi.searchCityAdCode(strKey);
    }

    public CityDataInfo getCityInfo(int adCode) {
        return mMapDataApi.getCityInfo(adCode);
    }

    public AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode) {
        return mMapDataApi.getAreaExtraInfo(adminCode);
    }

    public ArrayList<Integer> getAdCodeList(int downLoadMode, String strKey) {
        return mMapDataApi.getAdCodeList(downLoadMode, strKey);
    }

    public ArrayList<Integer> getWorkingQueueAdCodeList() {
        return mMapDataApi.getWorkingQueueAdCodeList();
    }

    public ArrayList<CityDataInfo> getWorkingList() {
        return mMapDataApi.getWorkingList();
    }

    public ArrayList<CityDataInfo> getWorkedList() {
        return mMapDataApi.getWorkedList();
    }

    public ArrayList<CityDataInfo> getNearAdCodeList(int adCode) {
        return mMapDataApi.getNearAdCodeList(adCode);
    }

    public int getAdCodeByLonLat(double lon, double lat) {
        return mMapDataApi.getAdCodeByLonLat(lon, lat);
    }

    public void deleteErrorData(int id) {
        mMapDataApi.deleteErrorData(id);
    }

    public void operate(int opType, ArrayList<Integer> adCodeDiyLst) {
        mMapDataApi.operate(opType, adCodeDiyLst);
    }

    public void registerCallBack(String key, MapDataAdapterCallBack callBack) {
        mMapDataApi.registerCallBack(key, callBack);
    }

    public void removeCallBack(String key, MapDataAdapterCallBack callBack) {
        mMapDataApi.unRegisterCallback(key, callBack);
    }

    public void unInitMapDataService() {
        mMapDataApi.unInit();
    }

    public static MapDataAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final MapDataAdapter ra = new MapDataAdapter();
    }
}
