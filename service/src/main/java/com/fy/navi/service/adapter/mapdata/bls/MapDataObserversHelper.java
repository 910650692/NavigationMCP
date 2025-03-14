package com.fy.navi.service.adapter.mapdata.bls;

import android.annotation.SuppressLint;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.data.MapDataService;
import com.autonavi.gbl.data.model.AdminCode;
import com.autonavi.gbl.data.model.Area;
import com.autonavi.gbl.data.model.AreaExtraInfo;
import com.autonavi.gbl.data.model.AreaType;
import com.autonavi.gbl.data.model.CityDownLoadItem;
import com.autonavi.gbl.data.model.CityItemInfo;
import com.autonavi.gbl.data.model.DataInitParam;
import com.autonavi.gbl.data.model.DownLoadMode;
import com.autonavi.gbl.data.model.InitConfig;
import com.autonavi.gbl.data.model.MapDataMode;
import com.autonavi.gbl.data.model.MergedStatusInfo;
import com.autonavi.gbl.data.model.ProvinceInfo;
import com.autonavi.gbl.data.model.TaskStatusCode;
import com.autonavi.gbl.data.observer.IDataInitObserver;
import com.autonavi.gbl.data.observer.IDataListObserver;
import com.autonavi.gbl.data.observer.IDownloadObserver;
import com.autonavi.gbl.data.observer.IErrorDataObserver;
import com.autonavi.gbl.data.observer.IMergedStatusInfoObserver;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.adapter.mapdata.MapDataAdapterCallBack;
import com.fy.navi.service.define.bean.AdMapPointBean;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * MapDataService辅助类.
 *
 * @Description Helper类只做对象及数据转换，不做原子能力调用
 * @Author fh
 * @date 2024/12/12
 */
public class MapDataObserversHelper implements IDataInitObserver, IDownloadObserver, IMergedStatusInfoObserver, IErrorDataObserver, IDataListObserver {
    private static final String TAG = MapDataObserversHelper.class.getSimpleName();
    private static final float DOWNLOAD_COMPLETED = 100.0f;
    private MapDataService mMapDataService;
    public int mInitCode;
    /*** 该集合只存放离线数据结果回调 key = 哪个类请求的，value = requestResult 理论上集合的长度永远为1**/
    private Hashtable<String, MapDataAdapterCallBack> mapDataResultObserverHashtable;

    protected MapDataObserversHelper(MapDataService mapDataService) {
        mapDataResultObserverHashtable = new Hashtable<>();
        mMapDataService = mapDataService;
    }

    protected void initMapDataService() {
        InitConfig config = new InitConfig();
        //下载路径，设置后没用
        config.strStoredPath = "";
        //all_city_compile.json、global.db文件目录
        config.strConfigfilePath = GBLCacheFilePath.OFFLINE_CONF_PATH;
        //数据下载存放路径，目前仅地图下载支持该参数配置，语音下载暂不支持
        config.strDownloadPath = GBLCacheFilePath.OFFLINE_DOWNLOAD_DIR;
        ArrayList<DataInitParam> extendedList = new ArrayList<>(1);
        config.extendedParamList = extendedList;
        // 设置磁盘空间安全阈值（默认设置为80MB）
        config.nThresholdValue = 80;
        // 设置地图数据模式,默认为：基础地图数据模式
        config.mapDataMode = MapDataMode.MAP_DATA_MODE_BASE;
        // 初始化离线地图数据服务
        mInitCode = mMapDataService.init(config, this);
        Logger.d(TAG, "initService: mInitCode = " + mInitCode);

        mMapDataService.addNetDownloadObserver(this);
        //设置异常数据监听观察者
        mMapDataService.setErrorDataObserver(this);
        mMapDataService.setIMergedStatusInfoObserver(this);
    }

    public void registerCallBack(String key, MapDataAdapterCallBack callBack) {
        mapDataResultObserverHashtable.put(key, callBack);
    }

    public void unRegisterCallback(String key, MapDataAdapterCallBack callBack) {
        mapDataResultObserverHashtable.remove(key, callBack);
    }

    public void removeCallback() {
        mapDataResultObserverHashtable.clear();
    }

    /**
     * 获取基础功能包数据
     * @return
     */
    public CityDataInfo getCountryData() {
        if (mMapDataService != null) {
            CityDataInfo cityDataInfo = new CityDataInfo();
            // 基础功能包adcode（值固定为0） 目前国家类型行政编码，仅为一个元素，值固定为0
            Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, 0);
            GsonUtils.copyBean(cityArea, cityDataInfo);
            // 获取城市数据包的下载状态信息
            cityDataInfo.downLoadInfo = getCityDownLoadInfo(cityArea.adcode);
            return cityDataInfo;
        } else {
            return null;
        }
    }

    /**
     * 获取直辖市数据
     * @return
     */
    public ProvDataInfo getDirectDataInfo() {
        if (mMapDataService != null) {
            ProvDataInfo provDataInfo = new ProvDataInfo();
            // 获取直辖市adcode列表  [110000,500000,310000,120000],按拼音顺序为北京市、重庆市、上海市、天津市
            ArrayList<Integer> adCodeDirectList = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_DIRECT);
            if (null != adCodeDirectList && !adCodeDirectList.isEmpty()) {
                // 获取对应直辖市下的城市列表
                ArrayList<CityDataInfo> directCityList = new ArrayList<>();
                for (int i = 0; i < adCodeDirectList.size(); i++) {
                    Integer cityAdcode = adCodeDirectList.get(i);
                    Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                    CityDataInfo cityDataInfo = new CityDataInfo();
                    GsonUtils.copyBean(cityArea, cityDataInfo);
                    // 获取城市数据包的下载状态信息
                    cityDataInfo.downLoadInfo = getCityDownLoadInfo( cityArea.adcode);
                    directCityList.add(cityDataInfo);
                }
                provDataInfo.name = "直辖市";
                provDataInfo.cityInfoList = directCityList;
            }
            return provDataInfo;
        } else {
            return null;
        }
    }

    /**
     * 获取所有省份+城市信息
     * @return
     */
    public ArrayList<ProvDataInfo> getMapDataList() {
        if (mMapDataService != null) {
            ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();

            provinceBeanList.add(getDirectDataInfo()); // 添加一级直辖市对应信息

            // 获取省份adCode列表（按拼音排序）
            ArrayList<Integer> adCodeProvLst = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_PROV);
            // 获取特别行政区adCode列表 [820000,810000,710000],按拼音顺序为澳门、香港、台湾
            ArrayList<Integer> adCodeSpecialLst = mMapDataService.getAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET, AreaType.AREA_TYPE_SPECIAL);
            ArrayList<Integer> allProvinceList = new ArrayList<>();
            allProvinceList.addAll(adCodeProvLst); // 添加一级省份对应信息
            allProvinceList.addAll(adCodeSpecialLst); // 添加一级特别行政区对应信息

            if (null != allProvinceList && !allProvinceList.isEmpty()) {
                for (int i = 0; i < allProvinceList.size(); i++) {
                    Integer provAdcode = allProvinceList.get(i);
                    Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    ProvDataInfo provDataInfo = new ProvDataInfo();
                    GsonUtils.copyBean(provArea, provDataInfo);

                    // 获取对应省份下的城市列表
                    ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                    ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                    if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                        for (int j = 0; j < subCityAdcodeList.size() - 1; j++) {
                            Integer cityAdcode = subCityAdcodeList.get(j);
                            Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                            CityDataInfo cityDataInfo = new CityDataInfo();
                            GsonUtils.copyBean(cityArea, cityDataInfo);

                            // 获取城市数据包的下载状态信息
                            cityDataInfo.downLoadInfo = getCityDownLoadInfo( cityArea.adcode);

                            cityBeanList.add(cityDataInfo);

                        }
                    }
                    provDataInfo.cityInfoList = cityBeanList;
                    provinceBeanList.add(provDataInfo);
                }
            }

            return provinceBeanList;
        } else {
            return null;
        }
    }

    /**
     * 获取城市数据包的下载状态信息
     * getCityDownLoadItem 离线地图数据下载专用接口
     * 下载模式downLoadMode=DOWNLOAD_MODE_NET时，需要等【首次】RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     * 下载模式downLoadMode=DOWNLOAD_MODE_USB时，需要每次等RequestDataListCheck请求的观察者监听pObserver回调OnRequestDataListCheck后调用。
     *
     * @param adCode
     * @return
     */
    public CityDownLoadInfo getCityDownLoadInfo(int adCode) {
        if (mMapDataService != null) {
            //通过adcode获取城市下载项信息
            CityDownLoadItem downloadItem = mMapDataService.getCityDownLoadItem(DownLoadMode.DOWNLOAD_MODE_NET, adCode);
            CityDownLoadInfo cityDownLoadBean = new CityDownLoadInfo();
            if (downloadItem != null) {
                GsonUtils.copyBean(downloadItem, cityDownLoadBean);
                // 客户端根据 downloadItem.bIsDataUsed 和 downloadItem.taskState 字段转换为前端需要显示的文案
                if (downloadItem.percent == DOWNLOAD_COMPLETED) {
                    cityDownLoadBean.statusTip = "已下载";
                } else {
                    cityDownLoadBean.statusTip = switchTaskStatusCodeToString(downloadItem.bIsDataUsed, downloadItem.taskState, downloadItem.IsCompltelyHighVer);
                }
            }
            return cityDownLoadBean;
        }
        return null;
    }

    /**
     * 通过adCode获取城市信息
     * @param adCode
     * @return
     */
    public CityDataInfo getCityInfo(int adCode) {
        if (mMapDataService != null) {
            CityItemInfo info = mMapDataService.getCityInfo(adCode);
            CityDataInfo cityInfo = new CityDataInfo();
            if (info != null) {
                cityInfo.upperAdcode = info.belongedProvince;
                cityInfo.name = info.cityName;
                cityInfo.areaType = info.cityLevel;
                cityInfo.adcode = info.cityAdcode;
                cityInfo.jianPin = info.initial;
                cityInfo.pinYin = info.pinyin;
                cityInfo.cityX = info.cityX;
                cityInfo.cityY = info.cityY;
                cityInfo.downLoadInfo = getCityDownLoadInfo(adCode);
            }
            Logger.d(TAG, "getCityInfo: cityInfo = " + GsonUtils.toJson(cityInfo));
            return cityInfo;
        }
        Logger.d(TAG, "getCityInfo: null");
        return null;
    }

    /**
     * 根据 downloadItem.bIsDataUsed 和 downloadItem.taskState 字段转换为前端需要显示的文案
     * @param isDataUsed
     * @param taskCode
     * @return
     */
    private String switchTaskStatusCodeToString(boolean isDataUsed, int taskCode, boolean IsCompltelyHighVer) {
        String desc = new String("");
        switch (taskCode) {
            case TaskStatusCode.TASK_STATUS_CODE_READY:
                if (isDataUsed) {
                    desc = "待更新";
                    if (IsCompltelyHighVer) {
                        // 全量更新，数据包待更新的大小用CityDownLoadItem.nFullZipSize字段值来显示
                    } else {
                        // 增量更新，数据包待更新的大小用CityDownLoadItem.nZipSize字段值来显示
                    }
                } else {
                    desc = "待下载";
                    // 待下载，数据包待下载的大小用CityDownLoadItem.nFullZipSize字段值来显示
                }
                break;
            case TaskStatusCode.TASK_STATUS_CODE_WAITING:
                desc = "等待中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_PAUSE:
                desc = "暂停";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_DOING:
            case TaskStatusCode.TASK_STATUS_CODE_DONE:
                desc = "下载中";
                if(isDataUsed) {
                    desc = "更新中";
                }
                break;
            case TaskStatusCode.TASK_STATUS_CODE_CHECKING:
                desc = "校验中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_CHECKED:
                desc = "校验完成";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_UNZIPPING:
                desc = "解压中";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_UNZIPPED:
                desc = "解压完成";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_SUCCESS:
                desc = "已下载";
                break;
            case TaskStatusCode.TASK_STATUS_CODE_ERR:
            case TaskStatusCode.TASK_STATUS_CODE_MAX:
                desc = "重试";
                break;
        }
        return desc;
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkingList() {
        ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
        if (mMapDataService != null) {
            ArrayList<Integer> adCodeList = mMapDataService.getWorkingQueueAdcodeList(DownLoadMode.DOWNLOAD_MODE_NET);
            if (null != adCodeList && !adCodeList.isEmpty()) {
                for (int i = 0; i < adCodeList.size(); i++) {
                    Integer cityAdCode = adCodeList.get(i);
                    Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                    CityDataInfo cityDataInfo = new CityDataInfo();
                    GsonUtils.copyBean(cityArea, cityDataInfo);
                    // 获取城市数据包的下载状态信息
                    cityDataInfo.downLoadInfo = getCityDownLoadInfo( cityArea.adcode);
                    if (cityDataInfo.downLoadInfo.taskState == TaskStatusCode.TASK_STATUS_CODE_WAITING // 等待中
                            || cityDataInfo.downLoadInfo.taskState == TaskStatusCode.TASK_STATUS_CODE_DOING
                            || cityDataInfo.downLoadInfo.taskState == TaskStatusCode.TASK_STATUS_CODE_DONE // 下载中
                            || cityDataInfo.downLoadInfo.taskState == TaskStatusCode.TASK_STATUS_CODE_PAUSE) {  // 暂停
                        // 下载中/更新中列表  等待中、下载中、暂停
                        cityBeanList.add(cityDataInfo);
                    }
                }
            }
        }
        return cityBeanList;
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        ArrayList<CityDataInfo> downloadedList = new ArrayList<>();
        ArrayList<CityDataInfo> dataInfos = getCityInfoList();
        if (null != dataInfos && !dataInfos.isEmpty()) {
            for (CityDataInfo info : dataInfos) {
                // 获取城市数据包的下载状态信息
                info.downLoadInfo = getCityDownLoadInfo(info.adcode);
                if (info.downLoadInfo.taskState == TaskStatusCode.TASK_STATUS_CODE_SUCCESS) {
                    // 已下载列表
                    downloadedList.add(info);
                }
            }
        }
        return downloadedList;
    }

    /**
     * 获取 已下载的 省份+城市 结构 信息
     * 该方法对外提供
     *
     * @return
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        if (mMapDataService == null) return null;

        ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();

        // 获取本地已存在数据的adcode列表信息
        ArrayList<Integer> downLoadAdcodeList =mMapDataService.getDownLoadAdcodeList();
        Logger.d(TAG, "getDownLoadAdCodeList: downLoadAdcodeList" + GsonUtils.toJson(downLoadAdcodeList));

        if (null != downLoadAdcodeList && !downLoadAdcodeList.isEmpty()) {

            //获取已下载省份code列表
            ArrayList<Integer> provAdcodeList = new ArrayList<>();
            for (int i = 0; i < downLoadAdcodeList.size(); i++) {
                Integer downLoadAdCode = downLoadAdcodeList.get(i);
                Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, downLoadAdCode);
                if (!provAdcodeList.contains(cityArea.upperAdcode)) {
                    provAdcodeList.add(cityArea.upperAdcode);
                }
            }

            //获取省份中已下载的城市信息
            for (int i = 0; i < provAdcodeList.size(); i++) {
                Integer provAdcode = provAdcodeList.get(i);
                //对省份信息进行赋值
                ProvDataInfo provDataInfo = new ProvDataInfo();
                if (provAdcode == 0) {
                    provDataInfo.name = "直辖市";
                    provDataInfo.areaType = 2;
                    provDataInfo.adcode = 0;
                    provDataInfo.jianPin = "zxs";
                    provDataInfo.pinYin = "zhixiashi";
                } else {
                    ProvinceInfo info = mMapDataService.getProvinceInfo(provAdcode);
                    provDataInfo.name = info.provName;
                    provDataInfo.areaType = info.provLevel;
                    provDataInfo.adcode = info.provAdcode;
                    provDataInfo.jianPin = info.provInitial;
                    provDataInfo.pinYin = info.provPinyin;
                }

                //获取省份下已下载的城市
                ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                for (int j = 0; j < downLoadAdcodeList.size(); j++) {
                    Integer downLoadAdCode = downLoadAdcodeList.get(j);

                    if (downLoadAdCode != 0) { // 移除基础功能包
                        Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, downLoadAdCode);
                        CityDataInfo cityDataInfo = new CityDataInfo();
                        GsonUtils.copyBean(cityArea, cityDataInfo);

                        if (cityDataInfo.upperAdcode == provDataInfo.adcode) {
                            cityBeanList.add(cityDataInfo);
                        }
                    }

                }

                provDataInfo.cityInfoList = cityBeanList;

                provinceBeanList.add(provDataInfo);

            }

        }

        Logger.d(TAG, "getDownLoadAdCodeList: provinceBeanList = " + GsonUtils.toJson(provinceBeanList));

        return provinceBeanList;
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return
     */
    public ArrayList<ProvDataInfo> searchAdCode(String strKey) {
        if (mMapDataService != null) {
            ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();
            // 获取adCode列表
            ArrayList<Integer> adCodeProvLst = mMapDataService.searchAdcode(strKey);
            if (null != adCodeProvLst && !adCodeProvLst.isEmpty()) {
                for (int i = 0; i < adCodeProvLst.size(); i++) {
                    Integer provAdcode = adCodeProvLst.get(i);
                    Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    if (provArea != null) {
                        ProvDataInfo provDataInfo = new ProvDataInfo();
                        GsonUtils.copyBean(provArea, provDataInfo);

                        // 一级列表非省份数据（直辖市+省辖市）
                        if(provDataInfo.areaType == 2 || provDataInfo.areaType == 3) {
                            // 获取一级城市数据包的下载状态信息(模糊搜索)
                            provDataInfo.downLoadInfo = getCityDownLoadInfo(provDataInfo.adcode);
                        }

                        ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                        ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                        if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                            for (int j = 0; j < subCityAdcodeList.size(); j++) {
                                Integer cityAdcode = subCityAdcodeList.get(j);
                                Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                                CityDataInfo cityDataInfo = new CityDataInfo();
                                GsonUtils.copyBean(cityArea, cityDataInfo);
                                // 获取城市数据包的下载状态信息
                                cityDataInfo.downLoadInfo = getCityDownLoadInfo( cityArea.adcode);

                                // 获取对应省份下的城市列表
                                cityBeanList.add(cityDataInfo);

                                provDataInfo.cityInfoList = cityBeanList; // 省份下所有城市列表

                            }
                        }

                        provinceBeanList.add(provDataInfo);
                    }

                }
            }
            return provinceBeanList;
        }
        Logger.d(TAG, "searchAdCode: null");
        return null;
    }

    /**
     * 通过搜索关键字获取已下载的行政区域adcode列表
     * 该方法对外提供使用
     *
     * @param strKey
     * @return
     */
    public ArrayList<ProvDataInfo> searchDownLoaded(String strKey) {
        if (mMapDataService != null) {
            //已下载数据集合（城市与省份同级）
            ArrayList<ProvDataInfo> provinceBeanList = new ArrayList<>();
            // 获取adCode列表
            ArrayList<Integer> adCodeProvLst = mMapDataService.searchAdcode(strKey);
            if (null != adCodeProvLst && !adCodeProvLst.isEmpty()) {
                for (int i = 0; i < adCodeProvLst.size(); i++) {
                    Integer provAdcode = adCodeProvLst.get(i);
                    Area provArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, provAdcode);
                    if (provArea != null) {
                        ProvDataInfo provDataInfo = new ProvDataInfo();
                        GsonUtils.copyBean(provArea, provDataInfo);

                        // 一级列表非省份数据（直辖市+省辖市）
                        if(provDataInfo.areaType == 2 || provDataInfo.areaType == 3) {
                            // 获取一级城市数据包的下载状态信息(模糊搜索)
                            provDataInfo.downLoadInfo = getCityDownLoadInfo(provDataInfo.adcode);
                            if (provDataInfo.downLoadInfo.percent == DOWNLOAD_COMPLETED) {
                                provinceBeanList.add(provDataInfo); // 匹配到的城市数据为已下载的直辖市和省辖市集合
                            }
                        }

                        ArrayList<CityDataInfo> cityBeanList = new ArrayList<>();
                        ArrayList<Integer> subCityAdcodeList = provArea.vecLowerAdcodeList;
                        if (null != subCityAdcodeList && !subCityAdcodeList.isEmpty()) {
                            for (int j = 0; j < subCityAdcodeList.size() - 1; j++) {
                                Integer cityAdcode = subCityAdcodeList.get(j);
                                Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdcode);
                                CityDataInfo cityDataInfo = new CityDataInfo();
                                GsonUtils.copyBean(cityArea, cityDataInfo);
                                // 获取城市数据包的下载状态信息
                                cityDataInfo.downLoadInfo = getCityDownLoadInfo(cityArea.adcode);

                                // 获取对应省份下已下载的城市列表
                                if (cityDataInfo.downLoadInfo.percent == DOWNLOAD_COMPLETED) {
                                    cityBeanList.add(cityDataInfo);
                                }
                                provDataInfo.cityInfoList = cityBeanList; // 省份下所有已下载城市列表

                            }
                        }
                        if (provDataInfo.cityInfoList != null && !provDataInfo.cityInfoList.isEmpty()) {
                            provinceBeanList.add(provDataInfo); // 匹配到的省份下面存在已下载的城市数据
                        }

                    }
                }
            }
            return provinceBeanList;
        }
        Logger.d(TAG, "searchDownloaded: null");
        return null;
    }

    /**
     * 通过搜索城市关键字获取行政区域adcode列表
     * @param strKey
     * @return
     */
    public int searchCityAdCode(String strKey) {
        if (mMapDataService != null) {
            ArrayList<Integer> adcode = mMapDataService.searchAdcode(strKey);
            if (null != adcode && !adcode.isEmpty()) {
                return adcode.get(0);
            }
        }
        Logger.d(TAG, "searchAdCode: null");
        return 0;
    }

    /**
     * 获取所有城市信息列表
     * @return
     */
    private ArrayList<CityDataInfo> getCityInfoList() {
        if (mMapDataService != null) {
            // 全部城市数据
            ArrayList<CityDataInfo> cityDataInfos = new ArrayList<>();
            ArrayList<CityItemInfo> cityItemInfos = mMapDataService.getCityInfoList();
            if (cityItemInfos == null) return null;
            for (CityItemInfo info : cityItemInfos) {
                CityDataInfo dataInfo = new CityDataInfo();
                dataInfo.upperAdcode = info.belongedProvince;
                dataInfo.name = info.cityName;
                dataInfo.areaType = info.cityLevel;
                dataInfo.adcode = info.cityAdcode;
                dataInfo.jianPin = info.initial;
                dataInfo.pinYin = info.pinyin;
                cityDataInfos.add(dataInfo);
            }
            Logger.d(TAG, "getCityInfoList: cityDataInfos = " + GsonUtils.toJson(cityDataInfos));
            return cityDataInfos;
        }
        Logger.d(TAG, "getCityInfoList is null");
        return null;
    }

    /**
     * 获取区域扩展信息
     * @param adminCode
     * @return
     */
    @SuppressLint("WrongConstant")
    public AreaExtraInfoBean getAreaExtraInfo(AdminCodeBean adminCode) {
        if (mMapDataService != null && adminCode != null) {
            AreaExtraInfoBean areaExtraInfoBean = new AreaExtraInfoBean();
            AreaExtraInfo areaExtraInfo = mMapDataService.getAreaExtraInfo(new AdminCode(adminCode.euRegionCode, adminCode.nCityAdCode, adminCode.nAdCode));
            if (areaExtraInfo != null) {
                areaExtraInfoBean.cityName = areaExtraInfo.cityName;
                areaExtraInfoBean.townName = areaExtraInfo.townName;
                areaExtraInfoBean.provName = areaExtraInfo.provName;
                areaExtraInfoBean.stAdCode = new AdminCodeBean(areaExtraInfo.stAdCode.euRegionCode,
                        areaExtraInfo.stAdCode.nCityAdCode, areaExtraInfo.stAdCode.nAdCode);
                areaExtraInfoBean.stCenterPoint = new AdMapPointBean(areaExtraInfo.stCenterPoint.nLon,
                        areaExtraInfo.stCenterPoint.nLat, areaExtraInfo.stCenterPoint.nZlevel);
            }
            return areaExtraInfoBean;
        }
        Logger.d(TAG, "getAreaExtraInfo: null");
        return null;
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(int adCode) {
        if (mMapDataService != null) {
            Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, adCode);
            ArrayList<Integer> vecNearAdCodeList = cityArea.vecNearAdcodeList;
            ArrayList<CityDataInfo> nearAdCodeList = new ArrayList<>();
            if (null != vecNearAdCodeList && !vecNearAdCodeList.isEmpty()) {
                for (int i = 0; i < vecNearAdCodeList.size(); i++) {
                    Integer cityAdCode = vecNearAdCodeList.get(i);
                    Area area = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, cityAdCode);
                    CityDataInfo cityDataInfo = new CityDataInfo();
                    GsonUtils.copyBean(area, cityDataInfo);
                    // 获取城市数据包的下载状态信息
                    cityDataInfo.downLoadInfo = getCityDownLoadInfo(area.adcode);
                    nearAdCodeList.add(cityDataInfo);
                }
            }
            Logger.d(TAG, "getNearAdCodeList: nearAdCodeList = " + GsonUtils.toJson(nearAdCodeList));
            return nearAdCodeList;
        }
        Logger.d(TAG, "getNearAdCodeList: null");
        return null;
    }

    @Override
    public void onInit(int downLoadMode, int dataType, int opCode) {
        Logger.d(TAG, "mMapDataService onInit: downLoadMode = " + downLoadMode
                + " dataType = " + dataType + " opCode = " + opCode);
        if (opCode == Service.ErrorCodeOK) {
            // 初始化成功，继续操作
            Logger.d(TAG, "MapDataService 初始化成功");
            if (mMapDataService != null) {
                // 发起云端数据列表检测需要在初始化观察者IDataInitObserver的回调函数OnInit触发后发起
                mMapDataService.requestDataListCheck(DownLoadMode.DOWNLOAD_MODE_NET, "", this);
            }
        } else {
            // 初始化失败，其他处理
            Logger.d(TAG, "MapDataService 初始化失败");
        }
    }

    @Override
    public void onRequestDataListCheck(int downLoadMode, int dataType, int opCode) {
        // 若有需要操作UI，请转UI线程
        Logger.d(TAG, "onRequestDataListCheck : downLoadMode = " + downLoadMode
                + " dataType = " + dataType + " opCode = " + opCode);
    }

    /**
     * 下载操作回调
     *
     * @param downLoadMode
     * @param dataType
     * @param opType
     * @param opreatedIdList
     */
    @Override
    public void onOperated(int downLoadMode, int dataType, int opType, ArrayList<Integer> opreatedIdList) {
        Logger.d(TAG, "onOperated: downLoadMode = " + downLoadMode + "; dataType = " + dataType +
                "; opType = " + opType + "; opreatedIdList = " + GsonUtils.toJson(opreatedIdList));
    }

    /**
     * 下载状态回调
     *
     * @param downLoadMode 下载模式
     * @param dataType 数据类型。 当dataType参数值为DATA_TYPE_MAP=0时，id参数为城市行政编码adcode值。
     * @param id  数据id
     * @param taskCode  任务状态
     * @param opCode 	操作状态码
     */
    @Override
    public void onDownLoadStatus(int downLoadMode, int dataType, int id, int taskCode, int opCode) {
        Logger.d(TAG, "onDownLoadStatus: downLoadMode = " + downLoadMode + "; dataType = " + dataType +
                "; id = " + id + "; taskCode = " + taskCode + "; opCode = " + opCode);

       /* //获取下载城市信息
        List<CityDataInfo> cityList = new ArrayList<>();
        if (mMapDataService != null) {
            CityDataInfo cityDataInfo = new CityDataInfo();
            Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, id);
            GsonUtils.copyBean(cityArea, cityDataInfo);
            cityDataInfo.downLoadInfo = getCityDownLoadInfo(cityArea.adcode);
            cityList.add(cityDataInfo);
        }

        ProvDataInfo provDataInfo = new ProvDataInfo();
        provDataInfo.cityInfoList = cityList; // 省份下所有城市列表*/

        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) return;
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) continue;
            callBack.onDownLoadStatus(null);
        }
    }

    /**
     * 下载进度百分比回调
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param percentType 0-表示下载进度，1-表示解压进度。
     * @param percent
     */
    @Override
    public void onPercent(int downLoadMode, int dataType, int id, int percentType, float percent) {
        Logger.d(TAG, "onPercent: downLoadMode = " + downLoadMode + "; dataType = " + dataType + "; percentType = " +
                percentType + "; id = " + id + "; percent = " + percent);

        //  0-表示下载进度，1-表示解压进度。
        if (percentType == 0) {
            //获取下载城市信息
            List<CityDataInfo> cityList = new ArrayList<>();
            if (mMapDataService != null) {
                CityDataInfo cityDataInfo = new CityDataInfo();
                Area cityArea = mMapDataService.getArea(DownLoadMode.DOWNLOAD_MODE_NET, id);
                GsonUtils.copyBean(cityArea, cityDataInfo);
                cityDataInfo.downLoadInfo = getCityDownLoadInfo(cityArea.adcode);
                cityList.add(cityDataInfo);
            }

            ProvDataInfo provDataInfo = new ProvDataInfo();
            provDataInfo.cityInfoList = cityList; // 省份下所有城市列表

            if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) return;
            for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
                if (callBack == null) continue;
                callBack.onPercent(provDataInfo);
            }
        }

    }

    /**
     * 引擎数据融合状态信息回调
     *
     * @param mergedStatusInfo
     */
    @Override
    public void onMergedStatusInfo(MergedStatusInfo mergedStatusInfo) {
        Logger.d(TAG, "onMergedStatusInfo: mergedStatusInfo = " + GsonUtils.toJson(mergedStatusInfo));
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) return;
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) continue;
            MergedStatusBean mergedStatusBean = new MergedStatusBean();
            GsonUtils.copyBean(mergedStatusInfo, mergedStatusBean);

            callBack.onMergedStatusInfo(mergedStatusBean);
        }
    }

    /**
     * 数据异常通知
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param errType
     * @param errMsg
     */
    @Override
    public void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg) {
        Logger.d(TAG, "onErrorNotify: downLoadMode = " + downLoadMode + "; dataType = " + dataType +
                "; id = " + id + "; errType = " + errType + "; errMsg = " + errMsg);
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) return;
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) continue;
            callBack.onErrorNotify(downLoadMode, dataType, id, errType, errMsg);
        }
    }

    /**
     * 异常数据清除回调通知
     *
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param opCode
     */
    @Override
    public void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode) {
        Logger.d(TAG, "onDeleteErrorData: downLoadMode = " + downLoadMode + "; dataType = " + dataType +
                "; id = " + id + "; opCode = " + opCode);
        if (ConvertUtils.isEmpty(mapDataResultObserverHashtable)) return;
        for (MapDataAdapterCallBack callBack : mapDataResultObserverHashtable.values()) {
            if (callBack == null) continue;
            callBack.onDeleteErrorData(downLoadMode, dataType, id, opCode);
        }
    }

}
