package com.fy.navi.service.adapter.user.usertrack.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.user.model.BehaviorDataType;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.usertrack.UserTrackService;
import com.autonavi.gbl.user.usertrack.model.BehaviorFileType;
import com.autonavi.gbl.user.usertrack.model.GpsTrackDepthInfo;
import com.autonavi.gbl.user.usertrack.model.GpsTrackPoint;
import com.autonavi.gbl.user.usertrack.model.SearchHistoryItem;
import com.autonavi.gbl.user.usertrack.observer.IGpsInfoGetter;
import com.autonavi.gbl.user.usertrack.observer.IUserTrackObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackPointBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.lang.reflect.Type;
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

    protected UserTrackImplHelper(final UserTrackService userTrackService) {
        mUserTrackResultHashtable = new Hashtable<>();
        mUserTrackService = userTrackService;
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    /**
     * 注册回调
     * @param key      回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final UserTrackAdapterCallBack callBack) {
        mUserTrackResultHashtable.put(key, callBack);
    }

    /**
     * 移除回调
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
     * @param item 搜索历史记录
     * @return 搜索历史记录bean
     */
    private SearchHistoryItemBean getSearchHistoryItem(final SearchHistoryItem item) {
        final SearchHistoryItemBean bean = new SearchHistoryItemBean();
        bean.setName(item.name);
        bean.setPoiid(item.poiid);
        bean.setId(item.id);
        bean.setType(item.type);
        bean.setDatatype(item.datatype);
        bean.setX(item.x);
        bean.setY(item.y);
        bean.setXentr(item.x_entr);
        bean.setYentr(item.y_entr);
        bean.setUpdateTime(item.update_time);
        bean.setHistoryType(item.history_type);
        bean.setIconinfo(item.iconinfo);
        bean.setAdcode(item.adcode);
        bean.setDistrict(item.district);
        bean.setAddress(item.address);
        bean.setPoiTag(item.poi_tag);
        bean.setFuncText(item.func_text);
        bean.setShortName(item.short_name);
        bean.setDisplayInfo(item.display_info);
        bean.setSearchQuery(item.search_query);
        bean.setTerminals(item.terminals);
        bean.setIgnoreDistrict(item.ignore_district);
        bean.setSearchTag(item.search_tag);
        bean.setSearchQuerySet(item.search_query_set);
        bean.setRichRating(item.rich_rating);
        bean.setNumReview(item.num_review);
        bean.setCategory(item.category);
        bean.setSuperAddress(item.super_address);
        bean.setDatatypeSpec(item.datatype_spec);
        bean.setPoi(item.poi);
        bean.setCitycode(item.citycode);
        bean.setVersion(item.version);
        bean.setParent(item.parent);
        bean.setChildType(item.childType);
        bean.setTowardsAngle(item.towardsAngle);
        bean.setFloorNo(item.floorNo);
        bean.setEndPoiExtension(item.endPoiExtension);
        return bean;
    }

    /**
     * 添加搜索历史记录
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
     * @param bean 搜索历史记录bean
     * @return 搜索历史记录
     */
    private SearchHistoryItem getSearchHistoryItem(final SearchHistoryItemBean bean) {
        final SearchHistoryItem item = new SearchHistoryItem();
        item.name = bean.getName();
        item.poiid = bean.getPoiid();
        item.id = bean.getId();
        item.type = bean.getType();
        item.datatype = bean.getDatatype();
        item.x = bean.getX();
        item.y = bean.getY();
        item.x_entr = bean.getXentr();
        item.y_entr = bean.getYentr();
        item.update_time = bean.getUpdateTime();
        item.history_type = bean.getHistoryType();
        item.iconinfo = bean.getIconinfo();
        item.adcode = bean.getAdcode();
        item.district = bean.getDistrict();
        item.address = bean.getAddress();
        item.poi_tag = bean.getPoiTag();
        item.func_text = bean.getFuncText();
        item.short_name = bean.getShortName();
        item.display_info = bean.getDisplayInfo();
        item.search_query = bean.getSearchQuery();
        item.terminals = bean.getTerminals();
        item.ignore_district = bean.getIgnoreDistrict();
        item.search_tag = bean.getSearchTag();
        item.search_query_set = bean.getSearchQuerySet();
        item.rich_rating = bean.getRichRating();
        item.num_review = bean.getNumReview();
        item.category = bean.getCategory();
        item.super_address = bean.getSuperAddress();
        item.datatype_spec = bean.getDatatypeSpec();
        item.poi = bean.getPoi();
        item.citycode = bean.getCitycode();
        item.version = bean.getVersion();
        item.parent = bean.getParent();
        item.childType = bean.getChildType();
        item.towardsAngle = bean.getTowardsAngle();
        item.floorNo = bean.getFloorNo();
        item.endPoiExtension = bean.getEndPoiExtension();
        return item;
    }

    /**
     * 删除搜索历史记录
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
            // 获取同步库轨迹文件
            final String id = dataBean.getId();
            final String filePath = mUserTrackService.getFilePath(type, id, BehaviorFileType.BehaviorFileTrail);// 车机版只有 轨迹文件(2) 生效
            dataBean.setFilePath(filePath);
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
                history.setMEndTime(dataBean.getEndTime()); // 该行程完成时间
                history.setMRideRunType(dataBean.getRideRunType()); // 行程类型（导航/巡航）
                history.setMTimeInterval(dataBean.getTimeInterval()); // 驾驶时长
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
     * 行程数据解析
     * @param jsonStr json串
     * @return 行程数据
     */
    public DrivingRecordDataBean parseJsonToBean(final String jsonStr) {
        final Gson gson = new GsonBuilder()
                .registerTypeAdapter(DrivingRecordDataBean.class, new JsonDeserializer<DrivingRecordDataBean>() {
                    @Override
                    public DrivingRecordDataBean deserialize(final JsonElement json, final Type typeOfT, final JsonDeserializationContext context) {
                        final JsonObject jsonObject = json.getAsJsonObject();
                        final DrivingRecordDataBean bean = new DrivingRecordDataBean();
                        // 基础字段
                        bean.setId(jsonObject.get("id").getAsString());
                        bean.setType(jsonObject.get("type").getAsInt());
                        bean.setRideRunType(jsonObject.get("rideRunType").getAsInt());
                        bean.setTimeInterval(jsonObject.get("timeInterval").getAsInt());
                        bean.setRunDistance(jsonObject.get("runDistance").getAsInt());
                        bean.setStartTime(jsonObject.get("startTime").getAsString());
                        bean.setEndTime(jsonObject.get("endTime").getAsString());
                        bean.setStartPoiName(jsonObject.get("startPoiName").getAsString());
                        bean.setEndPoiName(jsonObject.get("endPoiName").getAsString());
                        bean.setStartLocation(jsonObject.get("startLocation").getAsString());
                        bean.setEndLocation(jsonObject.get("endLocation").getAsString());
                        bean.setTrackFileName(jsonObject.get("trackFileName").getAsString());
                        bean.setMaxSpeedTime(jsonObject.get("maxSpeedTime").getAsString());
                        bean.setMaxSpeedLocation(jsonObject.get("maxSpeedLocation").getAsString());
                        bean.setMaxSpeedPoiName(jsonObject.get("maxSpeedPoiName").getAsString());
                        bean.setUpdateTime(jsonObject.get("updateTime").getAsInt());
                        if (jsonObject.get("trackFileMd5") != null) {
                            bean.setTrackFileMd5(jsonObject.get("trackFileMd5").getAsString());
                        }
                        if (jsonObject.get("trackPointsURL") != null) {
                            bean.setTrackPointsURL(jsonObject.get("trackPointsURL").getAsString());
                        }
                        // 特殊类型处理
                        final double maxSpeed = jsonObject.get("maxSpeed").getAsDouble();
                        bean.setMaxSpeed((int) Math.round(maxSpeed));
                        return bean;
                    }
                })
                .create();
        return gson.fromJson(jsonStr, DrivingRecordDataBean.class);
    }


    /**
     * 获取平均速度
     *
     * @param dataBean 行程数据
     * @return 平均速度
     */
    private int getAverageSpeed(final DrivingRecordDataBean dataBean) {
        final int runDistance = dataBean.getRunDistance();
        final int timeInterval = dataBean.getTimeInterval();
        if (runDistance == 0 || timeInterval == 0) {
            return 0;
        }
        return (int) ((double) runDistance / timeInterval * 3.6);
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
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
                dataBean.setEndTime(history.getMEndTime()); // 该行程完成时间
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
                dataBean.setEndTime(history.getMEndTime()); // 该行程完成时间
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
        pointBean.setF64Latitude(point.f64Latitude);
        pointBean.setF64Longitude(point.f64Longitude);
        pointBean.setF64Altitude(point.f64Altitude);
        pointBean.setF32Accuracy(point.f32Accuracy);
        pointBean.setF32Speed(point.f32Speed);
        pointBean.setF32Course(point.f32Course);
        pointBean.setN64TickTime(point.n64TickTime);
        pointBean.setN32SateliteTotal(point.n32SateliteTotal);
        pointBean.setSectionId(point.nSectionId);
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
