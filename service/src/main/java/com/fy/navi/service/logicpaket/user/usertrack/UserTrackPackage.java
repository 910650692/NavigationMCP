package com.fy.navi.service.logicpaket.user.usertrack;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;
import java.util.List;

public final class UserTrackPackage implements UserTrackAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final UserTrackAdapter mUserTrackAdapter;
    private final List<UserTrackCallBack> mCallBacks = new ArrayList<>();

    private UserTrackPackage() {
        mUserTrackAdapter = UserTrackAdapter.getInstance();
    }

    /**
     * 初始化服务
     */
    public void initUserTrackService() {
        mUserTrackAdapter.initUserTrackService();
        mUserTrackAdapter.registerCallBack("UserTrackPackage", this);
    }

    /**
     * 注册回调
     * @param callback 回调
     */
    public synchronized void registerCallBack(final UserTrackCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.add(callback);
        }
    }

    /**
     * 反初始化服务
     */
    public void unInitUserTrackService() {
        mUserTrackAdapter.unInitUserTrackService();
    }

    /**
     * 获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        return mUserTrackAdapter.getSearchHistory();
    }

    /**
     * 添加搜索历史记录
     * @param item 搜索历史记录
     * @return 添加结果
     */
    public int addSearchHistory(final SearchHistoryItemBean item) {
        /*// 同步模式
        int mode = UserTrackModeType.SyncModeNow; // 立即同步
        // 2.1 搜索历史 关键字
        SearchHistoryItem item = new SearchHistoryItem();
        item.name = "高德";
        item.datatype_spec = "0"; // 可以自己定义 ICON 标记
        // 2.2 搜索历史 POI
        SearchHistoryItem item1 = new SearchHistoryItem();
        item1.name = "肯德基";
        item1.category = "50301";
        item1.datatype_spec = "1"; // 可以自己定义 ICON 标记*/
        return mUserTrackAdapter.addSearchHistory(item);
    }

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    public int delSearchHistory(final String name) {
        // name = "肯德基";
        return mUserTrackAdapter.delSearchHistory(name);
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mUserTrackAdapter.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mUserTrackAdapter.getDrivingRecordDataList();
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return 行程数据列表
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mUserTrackAdapter.getDrivingRecordCruiseDataList();
    }

    /**
     * 获取所有行程总里程
     * @return 所有行程总里程
     */
    public int getTotalDuration() {
        return mUserTrackAdapter.getTotalDuration();
    }

    /**
     * 获取所有行程总时长
     * @return 所有行程总时长
     */
    public int getTotalDistance() {
        return mUserTrackAdapter.getTotalDistance();
    }

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param trailDriveDataId 行程ID
     * @param data 行程信息
     * @return 设置结果
     */
    public int setBehaviorData(final String trailDriveDataId, final String data) {
        return mUserTrackAdapter.setBehaviorData(trailDriveDataId, data);
    }

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     * @return 删除结果
     */
    public int delBehaviorData(final String id) {
        return mUserTrackAdapter.delBehaviorData(id);
    }

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id 行程ID
     * @return 文件路径
     */
    public String getFilePath(final String id) {
        return mUserTrackAdapter.getFilePath(id);
    }

    /**
     * 启动Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param un32MsecRate 打点频率（单位毫秒，值不低于500），如传入5000表示，每5000毫秒打一个GPS点
     * @return 结果值
     */
    public int startGpsTrack(final String psSavePath, final String psFileName, final long un32MsecRate) {
        final long nTimerRate = 2000; // 定时器触发间隔时间 单位:毫秒，不要低于 500
        // 返回 0 调用成功
        return mUserTrackAdapter.startGpsTrack(psSavePath, psFileName, nTimerRate);
    }

    /**
     * 关闭Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int closeGpsTrack(final String psSavePath, final String psFileName) {
        return mUserTrackAdapter.closeGpsTrack(psSavePath, psFileName);
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        return mUserTrackAdapter.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        for (UserTrackCallBack observer : mCallBacks) {
            observer.notify(eventType, exCode);
        }
    }

    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {
        // 可以调用 setBehaviorData 接口 保存未完成的行程数据(没有终点信息)
        for (UserTrackCallBack observer : mCallBacks) {
            observer.onStartGpsTrack(n32SuccessTag, psSavePath, psFileName);
        }
    }

    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        // 1.判断轨迹数据是否有效， 基础版只保存 导航里程超过 5KM 的行程数据
        // 2.获取 开始打点时保存的 未完成的行程数据
        // 3.补全终点信息
        // 4.调用 setBehaviorData 接口 保存完成的 GPS轨迹信息
        for (UserTrackCallBack observer : mCallBacks) {
            observer.onCloseGpsTrack(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {
        // 1.显示轨迹信息
        for (UserTrackCallBack observer : mCallBacks) {
            observer.onGpsTrackDepInfo(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    public static UserTrackPackage getInstance() {
        return Helper.EP;
    }

    private static final class Helper {
        private static final UserTrackPackage EP = new UserTrackPackage();
    }

}
