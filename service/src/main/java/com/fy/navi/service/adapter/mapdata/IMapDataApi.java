package com.fy.navi.service.adapter.mapdata;

import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/09
 */
public interface IMapDataApi {

    void init();

    ArrayList<ProvDataInfo> getMapDataList();

    CityDataInfo getCountryData();

    CityDataInfo getCityInfo(int adCode);

    AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode);

    int getAdCodeByLonLat(double lon, double lat);

    ArrayList<ProvDataInfo> getAllDownLoadedList();

    ArrayList<Integer> getAdCodeList(int downLoadMode, String strKey);

    ArrayList<ProvDataInfo> searchAdCode(String strKey);

    ArrayList<ProvDataInfo> searchDownLoaded(String strKey);

    int searchCityAdCode(String strKey);

    ArrayList<Integer> getWorkingQueueAdCodeList();

    ArrayList<CityDataInfo> getWorkingList();

    ArrayList<CityDataInfo> getWorkedList();

    ArrayList<CityDataInfo> getNearAdCodeList(int adCode);

    String getDataFileVersion(int adCode);

    void setCurrentCityAdCode(int adCode);

    void abortDataListCheck(int downloadMode);

    void operate(int opType, ArrayList<Integer> adCodeDiyLst);

    void registerCallBack(String key, MapDataAdapterCallBack callBack);

    void unRegisterCallback(String key, MapDataAdapterCallBack callBack);

    void unInit();

    void deleteErrorData(int id);
}
