package com.fy.navi.service.adapter.mapdata.bls;

import android.net.NetworkCapabilities;

import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.data.MapDataService;
import com.autonavi.gbl.data.model.DownLoadMode;
import com.autonavi.gbl.data.model.MapDataFileType;
import com.autonavi.gbl.data.model.OperationType;
import com.autonavi.gbl.data.observer.IDownloadObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.mapdata.IMapDataApi;
import com.fy.navi.service.adapter.mapdata.MapDataAdapterCallBack;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;

/**
 * 离线地图数据服务.
 *
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataAdapterImpl implements IMapDataApi {
    private static final String TAG = MapDefaultFinalTag.MAP_DATA_SERVICE_TAG;
    private MapDataService mMapDataService;
    private MapDataObserversHelper mMapDataObserversHelper;

    public MapDataAdapterImpl() {
        mMapDataService = (MapDataService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.MapDataSingleServiceID);
        mMapDataObserversHelper = new MapDataObserversHelper(mMapDataService);
    }

    @Override
    public void init() {
        mMapDataObserversHelper.initMapDataService();
    }

    @Override
    public void registerCallBack(String key, MapDataAdapterCallBack callBack) {
        mMapDataObserversHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(String key, MapDataAdapterCallBack callBack) {
        mMapDataObserversHelper.unRegisterCallback(key, callBack);
    }

    @Override
    public void unInit() {
        if (mMapDataService != null) {
            mMapDataService.unInit();
            mMapDataService.removeNetDownloadObserver((IDownloadObserver) this);
            mMapDataObserversHelper.removeCallback();
            mMapDataService = null;
        }
    }

    /**
     * 根据经纬度解析获取城市adCode
     * 若解析失败,则返回0
     * 调用该函数只需要获取MapDataService即可，不要求初始化成功
     */
    @Override
    public int getAdCodeByLonLat(double lon, double lat) {
        if (null == mMapDataService) {
            return 0;
        }
        return mMapDataService.getAdcodeByLonLat(lon, lat);
    }

    /**
     * 通过adCode获取城市信息
     * 若解析失败,则返回null
     */
    @Override
    public CityDataInfo getCityInfo(int adCode) {
        return mMapDataObserversHelper.getCityInfo(adCode);
    }

    @Override
    public AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode) {
        return mMapDataObserversHelper.getAreaExtraInfo(adminCode);
    }

    /**
     * 获取离线数据版本号
     * @param adCode  当前城市的行政编码
     * @return
     */
    @Override
    public String getDataFileVersion(int adCode) {

        // 当前城市无离线数据，显示默认版本号0_000000
        String dataVersion = "0_000000";

        if (mMapDataService != null) {

            // 通过adCode获取各地图数据文件版本号
            String m1Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_MAP);
            String m2Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_ROUTE);
            String m3Version = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_POI);
            String m4_proVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_3D);
            String m5aVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_JV);
            String m5bVersion = mMapDataService.getDataFileVersion(adCode, MapDataFileType.MAP_DATA_TYPE_FILE_JVLINK);

            // m1.ans文件存在,在使用m1的版本号做为基准
            if (null != m1Version && !m1Version.isEmpty()) {
                dataVersion = m1Version + "_" + "1";
                if (m1Version.equals(m2Version)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                // 依次判断m3、m4_pro、m5a、m5b
                if (m1Version.equals(m3Version)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m1Version.equals(m4_proVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m1Version.equals(m5aVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m1Version.equals(m5bVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
            } else if (null != m2Version && !m2Version.isEmpty()) {  // m1.ans文件不存在,在使用m2的版本号做为基准
                dataVersion = m2Version + "_" + "1";
                if (m2Version.equals(m3Version)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m2Version.equals(m4_proVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m2Version.equals(m5aVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m2Version.equals(m5bVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
            } else if (null != m3Version && !m3Version.isEmpty()) {  // m2.ans文件不存在,在使用m3的版本号做为基准
                dataVersion = m3Version + "_" + "1";
                if (m3Version.equals(m4_proVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m3Version.equals(m5aVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m3Version.equals(m5bVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
            } else if (null != m4_proVersion && !m4_proVersion.isEmpty()) {  // m3.ans文件不存在,在使用m4的版本号做为基准
                dataVersion = m4_proVersion + "_" + "1";
                if (m4_proVersion.equals(m5aVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
                if (m4_proVersion.equals(m5bVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
            } else if (null != m5aVersion && !m5aVersion.isEmpty()) {  // m4.ans文件不存在,在使用m5a的版本号做为基准
                dataVersion = m5aVersion + "_" + "1";
                if (m5aVersion.equals(m5bVersion)) {
                    dataVersion = dataVersion + "1";
                } else {
                    dataVersion = dataVersion + "0";
                }
            } else if (null != m5bVersion && !m5bVersion.isEmpty()) {  // m5a.ans文件不存在,在使用m5b的版本号做为基准
                dataVersion = m5bVersion + "_" + "1";
            }
        }
        return dataVersion;
    }

    /**
     * 设置当前城市的行政编码
     */
    @Override
    public void setCurrentCityAdCode(int adCode) {
        if (mMapDataService != null) {
            mMapDataService.setCurrentCityAdcode(adCode);
        }
    }

    @Override
    public ArrayList<Integer> getAdCodeList(int downLoadMode, String strKey) {
        if (mMapDataService != null) {
            return mMapDataService.getAdcodeList(downLoadMode, strKey);
        }
        return null;
    }

    /**
     * 取得等待中、下载中、暂停、解压中、重试状态下的所有城市adCode列表
     * <p>
     * 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     */
    @Override
    public ArrayList<Integer> getWorkingQueueAdCodeList() {
        if (mMapDataService != null) {
            return mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
        }
        return null;
    }

    @Override
    public ArrayList<CityDataInfo> getWorkingList() {
        return mMapDataObserversHelper.getWorkingList();
    }

    @Override
    public ArrayList<CityDataInfo> getWorkedList() {
        return mMapDataObserversHelper.getWorkedList();
    }

    @Override
    public ArrayList<CityDataInfo> getNearAdCodeList(int adCode) {
        return mMapDataObserversHelper.getNearAdCodeList(adCode);
    }

    @Override
    public void operate(int opType, ArrayList<Integer> adCodeDiyLst) {

        // 如果当前城市的数据包正在解压，则暂停操作不会立即生效，需等待解压完成。
        if (opType == OperationType.OPERATION_TYPE_PAUSE) {
            Logger.d(TAG, "operate: operate = 触发暂停下载");
        }

        if (mMapDataService != null) {
            if (opType == OperationType.OPERATION_TYPE_DELETE || opType == OperationType.OPERATION_TYPE_CANCEL) {
                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, opType, adCodeDiyLst);
            } else {
                mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, opType, adCodeDiyLst);
                // 判断当前是否连接网络
//                if (NetWorkUtils.Companion.getInstance().getNetworkTypeValue() == NetworkCapabilities.TRANSPORT_WIFI) {
//                    mMapDataService.operate(DownLoadMode.DOWNLOAD_MODE_NET, opType, adCodeDiyLst);
//                } else {
//                    Logger.d(TAG, "无网络连接，请检查网络后重试");
//                }
            }
        }
    }

    @Override
    public ArrayList<ProvDataInfo> getMapDataList() {
        return mMapDataObserversHelper.getMapDataList();
    }

    @Override
    public CityDataInfo getCountryData() {
        return mMapDataObserversHelper.getCountryData();
    }

    @Override
    public ArrayList<ProvDataInfo> searchAdCode(String strKey) {
        return mMapDataObserversHelper.searchAdCode(strKey);
    }

    @Override
    public ArrayList<ProvDataInfo> searchDownLoaded(String strKey) {
        return mMapDataObserversHelper.searchDownLoaded(strKey);
    }

    @Override
    public int searchCityAdCode(String strKey) {
        return  mMapDataObserversHelper.searchCityAdCode(strKey);
    }

    @Override
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mMapDataObserversHelper.getAllDownLoadedList();
    }

    @Override
    public void abortDataListCheck(int downloadMode) {
        if (mMapDataService != null) {
            mMapDataService.abortRequestDataListCheck(downloadMode);
        }
    }

    /**
     *   删除异常城市数据
     * @param id
     */
    @Override
    public void deleteErrorData(int id) {
        if (mMapDataService != null) {
            mMapDataService.deleteErrorData(id);
        }
    }
}
