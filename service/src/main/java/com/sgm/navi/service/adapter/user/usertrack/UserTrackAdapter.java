package com.sgm.navi.service.adapter.user.usertrack;

import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;
import com.sgm.navi.service.greendao.CommonManager;

import java.util.ArrayList;
import java.util.Objects;


public final class UserTrackAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(UserTrackAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "UserTrackImpl";
    private final IUserTrackApi mUserTrackApi;
    private final CommonManager mCommonManager;

    private UserTrackAdapter() {
        mUserTrackApi = (IUserTrackApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }

    /**
     * 初始化服务
     */
    public void initUserTrackService() {
        mUserTrackApi.initUserTrackService();
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final UserTrackAdapterCallBack callBack) {
        mUserTrackApi.registerCallBack(key, callBack);
    }

    /**
     * 移除回调
     * @param key 回调key
     */
    public void removeCallBack(final String key) {
        mUserTrackApi.unRegisterCallback(key);
    }

    /**
     * 反初始化服务
     */
    public void unInitUserTrackService() {
        mUserTrackApi.unInitUserTrackService();
    }

    /**
     * 判断是否登录
     * @return 是否登录
     */
    public boolean isLogin() {
        final AccountProfileInfo info;
        final String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                return !TextUtils.isEmpty(info.getUid());
            }
        }
        return false;
    }

    /**
     * 获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    public  ArrayList<SearchHistoryItemBean> getSearchHistory(){
        return mUserTrackApi.getSearchHistory();
    }

    /**
     * 添加搜索历史记录
     * @param item 搜索历史记录
     * @return 添加结果
     */
    public int addSearchHistory(final SearchHistoryItemBean item){
        return mUserTrackApi.addSearchHistory(item);
    }

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    public int delSearchHistory(final String name){
        return mUserTrackApi.delSearchHistory(name);
    }

    /**
     * 删除所有搜索历史记录
     * @return 删除结果
     */
    public int clearSearchHistory(){
        return mUserTrackApi.clearSearchHistory();
    }

    /**
     * 获取历史路线列表
     * @return 历史路线列表
     */
    public ArrayList<HistoryRouteItemBean> getHistoryRoute() {
        return mUserTrackApi.getHistoryRoute();
    }

    /**
     * 添加历史路线
     * @param item 历史路线
     * @return 添加结果
     */
    public int addHistoryRoute(final HistoryRouteItemBean item) {
        return mUserTrackApi.addHistoryRoute(item);
    }

    /**
     * 删除历史路线
     * @param bean 历史路线名称
     * @return 删除结果
     */
    public int delHistoryRoute(final HistoryRouteItemBean bean) {
        return mUserTrackApi.delHistoryRoute(bean);
    }

    /**
     * 删除历史路线
     * @return 删除结果
     */
    public int clearHistoryRoute() {
        return mUserTrackApi.clearHistoryRoute();
    }

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param id 行程ID
     * @param data 行程信息
     * @return 设置结果
     */
    public int setBehaviorData(final String id, final String data) {
        return mUserTrackApi.setBehaviorData(id,data);
    }

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     * @return 删除结果
     */
    public int delBehaviorData(final String id) {
        return mUserTrackApi.delBehaviorData(id);
    }

    /**
     * 获取行为数据id列表
     * @return 行为数据id列表
     */
    public int[] getBehaviorDataIds() {
        return mUserTrackApi.getBehaviorDataIds();
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mUserTrackApi.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mUserTrackApi.getDrivingRecordDataList();
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mUserTrackApi.getDrivingRecordCruiseDataList();
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        return mUserTrackApi.getDrivingRecordDataFromSdk();
    }

    /**
     * 获取所有行程总里程
     * @return 所有行程总里程
     */
    public int getTotalDuration() {
        return mUserTrackApi.getTotalDuration();
    }

    /**
     * 获取所有行程总时长
     * @return 所有行程总时长
     */
    public int getTotalDistance() {
        return mUserTrackApi.getTotalDistance();
    }

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id 行程ID
     * @return 文件路径
     */
    public String getFilePath(final String id) {
        return mUserTrackApi.getFilePath(id);
    }

    /**
     * 启动Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param un32MsecRate 打点频率（单位毫秒，值不低于500），如传入5000表示，每5000毫秒打一个GPS点
     * @return 结果值
     */
    public int startGpsTrack(final String psSavePath, final String psFileName, final long un32MsecRate) {
        return mUserTrackApi.startGpsTrack(psSavePath, psFileName, un32MsecRate);
    }

    /**
     * 关闭Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int closeGpsTrack(final String psSavePath, final String psFileName) {
        return mUserTrackApi.closeGpsTrack(psSavePath, psFileName);
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        return mUserTrackApi.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    public static UserTrackAdapter getInstance() {
        return Helper.RA;
    }

    private static final class Helper {
        private static final UserTrackAdapter RA = new UserTrackAdapter();
    }
}
