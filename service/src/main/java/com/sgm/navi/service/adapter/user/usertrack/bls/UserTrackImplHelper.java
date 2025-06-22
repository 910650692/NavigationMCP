package com.sgm.navi.service.adapter.user.usertrack.bls;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.user.model.BehaviorDataType;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.usertrack.UserTrackService;
import com.autonavi.gbl.user.usertrack.model.GpsTrackDepthInfo;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.autonavi.gbl.user.usertrack.model.HistoryRouteItem;
import com.autonavi.gbl.user.usertrack.model.HistoryRoutePoiItem;
import com.autonavi.gbl.user.usertrack.model.SearchHistoryItem;
import com.autonavi.gbl.user.usertrack.observer.IGpsInfoGetter;
import com.autonavi.gbl.user.usertrack.observer.IUserTrackObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBeanAdapter;
import com.sgm.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.sgm.navi.service.define.user.usertrack.GpsTrackPointBean;
import com.sgm.navi.service.define.user.usertrack.HistoryPoiItemBean;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;


public class UserTrackImplHelper implements IUserTrackObserver, IGpsInfoGetter {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final Hashtable<String, UserTrackAdapterCallBack> mUserTrackResultHashtable;
    private final UserTrackService mUserTrackService;
    private final HistoryManager mHistoryManager;
    private final ArrayList<DrivingRecordDataBean> mGuideDataBeans = new ArrayList<>();
    private final ArrayList<DrivingRecordDataBean> mCruiseDataBeans = new ArrayList<>();
    private static final String MODEL_NAME = "行程历史";
    private final Gson mGson = new GsonBuilder()
            .registerTypeAdapter(DrivingRecordDataBean.class, new DrivingRecordDataBeanAdapter())
            .create();

    protected UserTrackImplHelper(final UserTrackService userTrackService) {
        mUserTrackResultHashtable = new Hashtable<>();
        mUserTrackService = userTrackService;
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    /**
     * 注册回调
     *
     * @param key      回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final UserTrackAdapterCallBack callBack) {
        mUserTrackResultHashtable.put(key, callBack);
    }

    /**
     * 移除回调
     *
     * @param key 回调key
     */
    public void unRegisterCallBack(final String key) {
        mUserTrackResultHashtable.remove(key);
    }

    /**
     * 移除所有回调
     */
    public void removeCallback() {
        mUserTrackResultHashtable.clear();
    }

    /**
     * 初始化服务
     */
    public void initUserTrackService() {
        //初始化参数待配置
        mUserTrackService.init(this);
        // 添加轨迹服务观察者
        mUserTrackService.addObserver(this);
    }

    /**
     * 获取搜索历史记录列表
     *
     * @return 搜索历史记录列表
     */
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        if (mUserTrackService == null) {
            return null;
        }
        if (mUserTrackService.getSearchHistory() == null) {
            return null;
        }
        final ArrayList<SearchHistoryItemBean> historyItems = new ArrayList<>();
        for (SearchHistoryItem item : mUserTrackService.getSearchHistory()) {
            final SearchHistoryItemBean bean = getSearchHistoryItem(item);
            historyItems.add(bean);
        }
        return historyItems;
    }

    /**
     * 转换历史记录数据
     *
     * @param item 搜索历史记录
     * @return 搜索历史记录bean
     */
    private SearchHistoryItemBean getSearchHistoryItem(final SearchHistoryItem item) {
        final SearchHistoryItemBean bean = new SearchHistoryItemBean();
        if (!ConvertUtils.isEmpty(item)) {
            bean.setName(item.name);
            bean.setUpdateTime(item.update_time);
            Logger.d(TAG, "getSearchHistoryItem: " + GsonUtils.toJson(bean));
        }
        return bean;
    }

    /**
     * 添加搜索历史记录
     *
     * @param bean 搜索历史记录
     * @return 添加结果
     */
    public int addSearchHistory(final SearchHistoryItemBean bean) {
        final SearchHistoryItem item = getSearchHistoryItem(bean);
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.addSearchHistory(item, SyncMode.SyncModeNow);
    }

    /**
     * 转换历史记录数据
     *
     * @param bean 搜索历史记录bean
     * @return 搜索历史记录
     */
    private SearchHistoryItem getSearchHistoryItem(final SearchHistoryItemBean bean) {
        final SearchHistoryItem item = new SearchHistoryItem();
        if (!ConvertUtils.isEmpty(bean)) {
            item.name = bean.getName();
            item.update_time = bean.getUpdateTime();
        }
        return item;
    }

    /**
     * 删除搜索历史记录
     *
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    public int delSearchHistory(final String name) {
        final SearchHistoryItem item = new SearchHistoryItem();
        item.name = name; //  "肯德基";  删除只需要赋值名字字段
        if (mUserTrackService == null) {
            return -1;
        }
        final int ret = mUserTrackService.delSearchHistory(item, SyncMode.SyncModeNow);
        Logger.i(TAG, "delSearchHistory ret = " + ret);
        return ret;
    }

    /**
     * 获取导航历史记录列表
     *
     * @return 导航历史记录列表
     */
    public ArrayList<HistoryRouteItemBean> getHistoryRoute() {
        if (mUserTrackService == null) {
            return null;
        }
        if (mUserTrackService.getHistoryRoute() == null) {
            return null;
        }
        final ArrayList<HistoryRouteItemBean> historyItems = new ArrayList<>();
        for (HistoryRouteItem item : mUserTrackService.getHistoryRoute()) {
            historyItems.add(getHistoryRouteItemBean(item));
        }
        return historyItems;
    }

    /**
     * 转换导航历史记录
     *
     * @param item 导航历史记录
     * @return 导航历史记录bean
     */
    private HistoryRouteItemBean getHistoryRouteItemBean(final HistoryRouteItem item) {
        final HistoryRouteItemBean bean = new HistoryRouteItemBean();
        if (!ConvertUtils.isEmpty(item)) {
            bean.setId(item.id);
            bean.setType(item.type);
            bean.setStartLoc(new GeoPoint(item.startLoc.lon, item.startLoc.lat));
            bean.setEndLoc(new GeoPoint(item.endLoc.lon, item.endLoc.lat));
            bean.setMethod(item.method);
            bean.setUpdateTime(item.updateTime);
            bean.setFromPoi(getHistoryPoiItemBean(item.fromPoi));
            bean.setToPoi(getHistoryPoiItemBean(item.toPoi));
            // 中间POI列表转换
            if (item.midPoi != null) {
                final ArrayList<HistoryPoiItemBean> midPois = new ArrayList<>();
                for (HistoryRoutePoiItem poi : item.midPoi) {
                    midPois.add(getHistoryPoiItemBean(poi));
                }
                bean.setMidPoi(midPois);
            }
            Logger.d(TAG, "getHistoryRouteItemBean: " + GsonUtils.toJson(bean));
        }
        return bean;
    }

    /**
     * 转换导航历史记录
     *
     * @param poi 导航历史记录
     * @return 导航历史记录bean
     */
    private HistoryRouteItem getHistoryPoiItem(final HistoryRouteItemBean poi) {
        final HistoryRouteItem item = new HistoryRouteItem();
        if (!ConvertUtils.isEmpty(poi)) {
            item.id = poi.getId();
            item.type = 302;//据高德反馈，在线时type固定为302
            item.updateTime = poi.getUpdateTime();
            item.startLoc = new Coord2DDouble(poi.getStartLoc().getLon(), poi.getStartLoc().getLat());
            item.endLoc = new Coord2DDouble(poi.getEndLoc().getLon(), poi.getEndLoc().getLat());
            item.method = poi.getMethod();
            item.fromPoi = getHistoryRoutePoiItem(poi.getFromPoi());
            item.toPoi = getHistoryRoutePoiItem(poi.getToPoi());
            if (poi.getMidPoi() != null) {
                final ArrayList<HistoryRoutePoiItem> midPois = new ArrayList<>();
                for (HistoryPoiItemBean poiItem : poi.getMidPoi()) {
                    midPois.add(getHistoryRoutePoiItem(poiItem));
                }
                item.midPoi = midPois;
            }
        }
        return item;
    }

    /**
     * 添加导航历史记录
     *
     * @param bean 导航历史记录
     * @return 添加结果
     */
    public int addHistoryRoute(final HistoryRouteItemBean bean) {
        if (mUserTrackService == null) {
            return -1;
        }
        final HistoryRouteItem item = getHistoryPoiItem(bean);
        final int code = mUserTrackService.addHistoryRoute(item, SyncMode.SyncModeNow);
        Logger.d(TAG, "addHistoryRoute ret = " + code + " bean = " + GsonUtils.toJson(bean));
        return code;
    }

    /**
     * 删除导航历史记录
     *
     * @param bean 导航历史记录
     * @return 删除结果
     */
    public int delHistoryRoute(final HistoryRouteItemBean bean) {
        if (mUserTrackService == null) {
            return -1;
        }
        final HistoryRouteItem item = getHistoryPoiItem(bean);
        final int code = mUserTrackService.delHistoryRoute(item, SyncMode.SyncModeNow);
        Logger.d(TAG, "delHistoryRoute ret = " + code + " bean = " + GsonUtils.toJson(bean));
        return code;
    }

    /**
     * 清空导航历史记录
     *
     * @return 删除结果
     */
    public int clearHistoryRoute() {
        if (mUserTrackService == null) {
            return -1;
        }
        final int code = mUserTrackService.clearHistoryRoute(SyncMode.SyncModeNow);
        Logger.d(TAG, "clearHistoryRoute ret = " + code);
        return code;
    }

    /**
     * 转换导航历史记录
     *
     * @param bean 导航历史记录
     * @return 导航历史记录bean
     */
    private HistoryRoutePoiItem getHistoryRoutePoiItem(final HistoryPoiItemBean bean) {
        final HistoryRoutePoiItem poi = new HistoryRoutePoiItem();
        if (!ConvertUtils.isEmpty(bean)) {
            poi.poiId = bean.getPoiId();
            poi.name = bean.getName();
        }
        return poi;
    }

    /**
     * 转换导航历史记录
     *
     * @param sourcePoi 导航历史记录
     * @return 导航历史记录bean
     */
    private HistoryPoiItemBean getHistoryPoiItemBean(final HistoryRoutePoiItem sourcePoi) {
        final HistoryPoiItemBean targetPoi = new HistoryPoiItemBean();
        if (!ConvertUtils.isEmpty(sourcePoi)) {
            targetPoi.setPoiId(sourcePoi.poiId);
            targetPoi.setTypeCode(sourcePoi.typeCode);
            targetPoi.setName(sourcePoi.name);
            targetPoi.setAddress(sourcePoi.address);
            targetPoi.setPoiLoc(new GeoPoint(sourcePoi.poiLoc.lon, sourcePoi.poiLoc.lat));
            targetPoi.setParent(sourcePoi.parent);
            targetPoi.setChildType(sourcePoi.childType);
            targetPoi.setTowardsAngle(sourcePoi.towardsAngle);
            targetPoi.setFloorNo(sourcePoi.floorNo);
            targetPoi.setEndPoiExtension(sourcePoi.endPoiExtension);
            targetPoi.setCityCode(sourcePoi.cityCode);
            targetPoi.setCityName(sourcePoi.cityName);
            if (sourcePoi.entranceList != null && !sourcePoi.entranceList.isEmpty()) {
                final ArrayList<GeoPoint> entrances = new ArrayList<>();
                for (Coord2DDouble loc : sourcePoi.entranceList) {
                    entrances.add(new GeoPoint(loc.lon, loc.lat));
                }
                targetPoi.setEntranceList(entrances);
            }
        }
        return targetPoi;
    }

    /**
     * 从sdk获取行程数据列表（同步数据到本地数据库）
     */
    public void getDrivingRecordData() {
        if (mUserTrackService == null) {
            return;
        }
        // 403  车机版只有 驾车轨迹(403) 生效
        final int type = BehaviorDataType.BehaviorTypeTrailDriveForAuto;
        // 获取行程ID列表（已完结记录）
        final int[] behaviorDataIds = mUserTrackService.getBehaviorDataIds(type);
        if (behaviorDataIds == null) {
            Logger.i(TAG, "behaviorDataIds is null");
            return;
        }
        Logger.i(TAG, "behaviorDataIds -> " + GsonUtils.toJson(behaviorDataIds));

        // 通过id获取行为数据
        for (int num : behaviorDataIds) {
            // 通过id获取行程数据Json串
            final String behaviorData = mUserTrackService.getBehaviorDataById(type, num);
            Logger.i(TAG, "behaviorData -> " + behaviorData);
            final DrivingRecordDataBean dataBean = parseJsonToBean(behaviorData);
            if (mHistoryManager.isDataExist(dataBean.getRideRunType(), dataBean.getId())) {
                continue;
            }
            if (dataBean != null) {
                final History history = new History();
                history.setMPoiId(dataBean.getId()); // 数据ID
                history.setMStartPoiName(dataBean.getStartPoiName()); // 设置起点
                history.setMEndPoiName(dataBean.getEndPoiName());// 设置终点
                history.setMStartPoint(dataBean.getStartLocation());
                history.setMEndPoint(dataBean.getEndLocation());
                history.setMRunDistance(dataBean.getRunDistance());// 该行程行驶距离
                history.setMStartTime(dataBean.getStartTime());
                history.setMRideRunType(dataBean.getRideRunType()); // 行程类型（导航/巡航）
                history.setMTimeInterval(Math.abs(dataBean.getTimeInterval())); // 驾驶时长
                history.setMAverageSpeed(getAverageSpeed(dataBean)); // 平均速度
                history.setMMaxSpeed(dataBean.getMaxSpeed()); // 最快速度
                history.setMTrackFileName(dataBean.getTrackFileName());
                history.setMFilePath(dataBean.getFilePath());
                history.setMType(AutoMapConstant.SearchKeywordRecordKey.DRIVING_HISTORY_RECORD_KEY); // 该条记录类型
                mHistoryManager.insertOrReplace(history);
            }
        }
    }

    /**
     * 解析json数据为DrivingRecordDataBean
     *
     * @param jsonStr json数据
     * @return DrivingRecordDataBean
     */
    public DrivingRecordDataBean parseJsonToBean(final String jsonStr) {
        if (TextUtils.isEmpty(jsonStr)) {
            Logger.i(TAG, "parseJsonToBean: 输入JSON为空");
            return null;
        }
        return mGson.fromJson(jsonStr, DrivingRecordDataBean.class);
    }

    /**
     * 获取平均速度
     *
     * @param dataBean 行程数据
     * @return 平均速度
     */
    private int getAverageSpeed(final DrivingRecordDataBean dataBean) {
        if (dataBean == null) {
            return 0;
        }
        final int runDistance = dataBean.getRunDistance();
        final int timeInterval = dataBean.getTimeInterval();
        if (runDistance <= 0 || timeInterval <= 0) {
            return 0;
        }
        return (int) ((runDistance * 3.6) / timeInterval);
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     *
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        if (mUserTrackService == null) {
            return null;
        }
        // 403  车机版只有 驾车轨迹(403) 生效
        final int type = BehaviorDataType.BehaviorTypeTrailDriveForAuto;
        // 获取行程ID列表（已完结记录）
        final int[] behaviorDataIds = mUserTrackService.getBehaviorDataIds(type);
        if (behaviorDataIds == null) {
            Logger.i(TAG, "behaviorDataIds is null");
            return null;
        }
        Logger.i(TAG, "behaviorDataIds -> " + GsonUtils.toJson(behaviorDataIds));
        final ArrayList<DrivingRecordDataBean> drivingRecordDataBeans = new ArrayList<>();
        // 通过id获取行为数据
        for (int num : behaviorDataIds) {
            // 通过id获取行程数据Json串
            final String behaviorData = mUserTrackService.getBehaviorDataById(type, num);
            Logger.i(TAG, "behaviorData -> " + behaviorData);
            final DrivingRecordDataBean dataBean = parseJsonToBean(behaviorData);
            drivingRecordDataBeans.add(dataBean);
        }
        return drivingRecordDataBeans;
    }

    /**
     * 获取导航行程历史数据
     *
     * @return 导航行程历史数据
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        mGuideDataBeans.clear();
        final List<History> list = mHistoryManager.getValueByType(2);
        if (list != null && !list.isEmpty()) {
            for (History history : list) {
                final DrivingRecordDataBean dataBean = new DrivingRecordDataBean();
                dataBean.setId(history.getMPoiId());// 数据ID
                dataBean.setStartPoiName(history.getMStartPoiName()); // 设置起点
                dataBean.setEndPoiName(history.getMEndPoiName()); // 设置终点
                dataBean.setStartLocation(history.getMStartPoint()); // 数据ID
                dataBean.setEndLocation(history.getMEndPoint()); // 数据ID
                dataBean.setRunDistance(history.getMRunDistance()); // 该行程行驶距离
                dataBean.setStartTime(history.getMStartTime());
                dataBean.setRideRunType(history.getMRideRunType()); // 行程类型（导航/巡航）
                dataBean.setTimeInterval(history.getMTimeInterval()); // 驾驶时长
                dataBean.setAverageSpeed(history.getMAverageSpeed()); // 平均速度
                dataBean.setMaxSpeed(history.getMMaxSpeed()); // 最快速度
                dataBean.setTrackFileName(history.getMTrackFileName());
                dataBean.setFilePath(history.getMFilePath());
                if (history.getMRideRunType() == 1) {
                    mGuideDataBeans.add(dataBean);
                }
            }
            Logger.i(TAG, "guideDataBeans -> " + GsonUtils.toJson(mGuideDataBeans));
        }
        return mGuideDataBeans;
    }

    /**
     * 获取巡航行程历史数据
     *
     * @return 巡航行程历史数据
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        mCruiseDataBeans.clear();
        final List<History> list = mHistoryManager.getValueByType(2);
        if (list != null && !list.isEmpty()) {
            for (History history : list) {
                final DrivingRecordDataBean dataBean = new DrivingRecordDataBean();
                dataBean.setId(history.getMPoiId());// 数据ID
                dataBean.setStartPoiName(history.getMStartPoiName()); // 设置起点
                dataBean.setEndPoiName(history.getMEndPoiName()); // 设置终点
                dataBean.setStartLocation(history.getMStartPoint()); // 数据ID
                dataBean.setEndLocation(history.getMEndPoint()); // 数据ID
                dataBean.setRunDistance(history.getMRunDistance()); // 该行程行驶距离
                dataBean.setStartTime(history.getMStartTime());
                dataBean.setRideRunType(history.getMRideRunType()); // 行程类型（导航/巡航）
                dataBean.setTimeInterval(history.getMTimeInterval()); // 驾驶时长
                dataBean.setAverageSpeed(history.getMAverageSpeed()); // 平均速度
                dataBean.setMaxSpeed(history.getMMaxSpeed()); // 最快速度
                if (history.getMRideRunType() == 0) {
                    mCruiseDataBeans.add(dataBean);
                }
            }
            Logger.i(TAG, "cruiseDataBeans -> " + GsonUtils.toJson(mCruiseDataBeans));
        }
        return mCruiseDataBeans;
    }

    /**
     * 获取轨迹数据同步回调通知
     *
     * @param eventType 同步SDK回调事件类型
     * @param exCode    同步SDK返回值
     */
    @Override
    public void notify(final int eventType, final int exCode) {
        Logger.i(TAG, "notify -> eventType = " + eventType + "; exCode = " + exCode);
        if (ConvertUtils.isEmpty(mUserTrackResultHashtable)) {
            return;
        }
        for (UserTrackAdapterCallBack callBack : mUserTrackResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.notify(eventType, exCode);
        }
    }

    /**
     * IGpsInfoGetter回调获取GPS信息
     *
     * @return GpsTrackPoint
     */
    @Override
    public GpsTrackPoint getGpsTrackPoint() {

        final GpsTrackPointBean bean = PositionPackage.getInstance().getGpsTrackPointBean();
        if (bean == null) {
            return null;
        }
        Logger.i(TAG, "getGpsTrackPoint -> " + GsonUtils.toJson(bean));
        final GpsTrackPoint gpsTrackPoint = new GpsTrackPoint();
        gpsTrackPoint.f64Latitude = bean.getF64Latitude();
        gpsTrackPoint.f64Longitude = bean.getF64Longitude();
        gpsTrackPoint.f32Speed = bean.getF32Speed();
        gpsTrackPoint.f32Course = bean.getF32Course();
        gpsTrackPoint.f64Altitude = bean.getF64Altitude();
        gpsTrackPoint.f32Accuracy = bean.getF32Accuracy();
        gpsTrackPoint.n64TickTime = bean.getN64TickTime();
        gpsTrackPoint.n32SateliteTotal = bean.getN32SateliteTotal();
        gpsTrackPoint.nSectionId = bean.getSectionId();

        return gpsTrackPoint;
    }

    /**
     * 开启Gps轨迹生成的回调通知
     *
     * @param psSavePath    GPS轨迹文件保存路径
     * @param psFileName    GPS轨迹文件名
     * @param n32SuccessTag 状态
     *                      -1 失败
     *                      0 成功：新建轨迹文件进行打点
     *                      1 成功：在已存在的轨迹文件继续追加打点
     */
    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {
        if (ConvertUtils.isEmpty(mUserTrackResultHashtable)) {
            return;
        }
        for (UserTrackAdapterCallBack callBack : mUserTrackResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            callBack.onStartGpsTrack(n32SuccessTag, psSavePath, psFileName);
        }
    }

    /**
     * 关闭Gps轨迹生成的回调通知
     *
     * @param n32SuccessTag 状态
     *                      -1 失败
     *                      0 成功：新建轨迹文件进行打点
     *                      1 成功：在已存在的轨迹文件继续追加打点
     * @param psSavePath    GPS轨迹文件保存路径
     * @param psFileName    GPS轨迹文件名
     * @param depInfo       轨迹文件信息
     */
    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthInfo depInfo) {
        if (ConvertUtils.isEmpty(mUserTrackResultHashtable)) {
            return;
        }
        for (UserTrackAdapterCallBack callBack : mUserTrackResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final GpsTrackDepthBean info = getGpsTrackDepthBean(depInfo);
            callBack.onCloseGpsTrack(n32SuccessTag, psSavePath, psFileName, info);
        }
    }

    /**
     * 数据转换深度信息
     *
     * @param depInfo 深度信息
     * @return 深度信息
     */
    private GpsTrackDepthBean getGpsTrackDepthBean(final GpsTrackDepthInfo depInfo) {

        if (depInfo == null || depInfo.trackPoints == null) {
            Logger.e(TAG, "无效的深度信息数据");
            return new GpsTrackDepthBean();
        }

        final GpsTrackDepthBean info = new GpsTrackDepthBean();
        info.setFileName(depInfo.fileName);
        info.setFilePath(depInfo.filePath);
        info.setFastestIndex(depInfo.fastestIndex);
        info.setDistance(depInfo.distance);
        info.setDuration(depInfo.duration);
        info.setAverageSpeed(depInfo.averageSpeed);

        final ArrayList<GpsTrackPointBean> getGpsTrackPointBean = new ArrayList<>();
        for (GpsTrackPoint point : depInfo.trackPoints) {
            getGpsTrackPointBean.add(getGpsTrackPointBean(point));
        }
        info.setTrackPoints(getGpsTrackPointBean);
        return info;
    }

    /**
     * 数据转换Gps轨迹点信息
     *
     * @param point Gps轨迹点信息
     * @return Gps轨迹点信息
     */
    private GpsTrackPointBean getGpsTrackPointBean(final GpsTrackPoint point) {
        final GpsTrackPointBean pointBean = new GpsTrackPointBean();
        if (!ConvertUtils.isEmpty(point)) {
            pointBean.setF64Latitude(point.f64Latitude);
            pointBean.setF64Longitude(point.f64Longitude);
            pointBean.setF64Altitude(point.f64Altitude);
            pointBean.setF32Accuracy(point.f32Accuracy);
            pointBean.setF32Speed(point.f32Speed);
            pointBean.setF32Course(point.f32Course);
            pointBean.setN64TickTime(point.n64TickTime);
            pointBean.setN32SateliteTotal(point.n32SateliteTotal);
            pointBean.setSectionId(point.nSectionId);
        }
        return pointBean;
    }

    /**
     * 获取Gps轨迹文件深度信息的回调通知
     *
     * @param n32SuccessTag 状态
     *                      -1 失败
     *                      0 成功：新建轨迹文件进行打点
     *                      1 成功：在已存在的轨迹文件继续追加打点
     * @param psSavePath    GPS轨迹文件保存路径
     * @param psFileName    GPS轨迹文件名
     * @param depInfo       轨迹文件信息
     */
    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthInfo depInfo) {
        Logger.i(TAG, "onGpsTrackDepInfo -> n32SuccessTag = " + n32SuccessTag + "; psSavePath = " + psSavePath +
                "; psFileName = " + psFileName + "; depInfo = " + GsonUtils.toJson(depInfo));
        if (ConvertUtils.isEmpty(mUserTrackResultHashtable)) {
            return;
        }
        for (UserTrackAdapterCallBack callBack : mUserTrackResultHashtable.values()) {
            if (callBack == null) {
                continue;
            }
            final GpsTrackDepthBean info = getGpsTrackDepthBean(depInfo);
            callBack.onGpsTrackDepInfo(n32SuccessTag, psSavePath, psFileName, info);
        }
    }

    /**
     * 校验账号数据服务状态.
     */
    protected void checkoutUserTrackServer() {
        if (ServiceInitStatus.ServiceNotInit == mUserTrackService.isInit()) {
            initUserTrackService();
        }
    }

    /**
     * 反初始化服务
     */
    public void unInitUserTrackService() {
        if (mUserTrackService != null) {
            removeCallback();
            if (ServiceInitStatus.ServiceInitDone != mUserTrackService.isInit()) {
                mUserTrackService.removeObserver(this);
                mUserTrackService.unInit();
            }
        }
    }

}
