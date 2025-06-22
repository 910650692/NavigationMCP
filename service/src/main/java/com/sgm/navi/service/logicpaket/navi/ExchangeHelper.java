package com.sgm.navi.service.logicpaket.navi;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviExchangeEntity;
import com.sgm.navi.service.define.navi.NaviInfoEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * 道路切换辅助类
 * @author sgm
 * @version $Revision.*$
 */
public class ExchangeHelper {

    public static final String TAG = "ExchangeHelper";

    private final NaviAdapter mNaviAdapter;

    public ExchangeHelper() {
        mNaviAdapter = NaviAdapter.getInstance();
    }

    /**
     * 获取道路交换结果
     * @param roadName 道路名称
     * @param exchangeType 交换类型 0:不走此道路 1:走此道路
     * @param mapTypeId    屏幕id
     * @return NaviExchangeEntity
     */
    public NaviExchangeEntity getExchangeResult(final String roadName, final int exchangeType,
                                                final MapType mapTypeId) {
        Logger.i(TAG, "getExchangeResult roadName:" + roadName +
                " exchangeType:" + exchangeType + "mapTypeId:" + mapTypeId);
        final NaviExchangeEntity naviExchangeEntity = new NaviExchangeEntity(NaviExchangeEntity.
                ExchangeType.NO_ROUTE, null);
        // 获取传入的道路所在的path和segment索引
        final ArrayList<RoadEntity> roadEntities = getRoadSegmentIdx(roadName);
        final long currentPathId = OpenApiHelper.getCurrentPathId(mapTypeId);
        // 不走此道路
        if (exchangeType == 0) {
            // 道路不存在
            if (ConvertUtils.isEmpty(roadEntities)) {
                naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.
                        ROAD_NOT_IN_CURRENT_ROUTE);
                return naviExchangeEntity;
            }
            final boolean isInCurrentRoute = isInCurrentRoute(currentPathId, roadEntities);
            if (!isInCurrentRoute) {
                // 不走道路，如果道路不在在当前路径中，就直接返回当前线路不包含此道路
                naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.
                        ROAD_NOT_IN_CURRENT_ROUTE);
                return naviExchangeEntity;
            } else {
                // 道路在当前路径中
                final boolean isHasPassed = isPassed(currentPathId, roadEntities);
                // 已经经过
                if (isHasPassed) {
                    naviExchangeEntity.setExchangeType(
                            NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED);
                    return naviExchangeEntity;
                } else {
                    // 未经过
                    return chooseOtherNoRoadPath(currentPathId, roadEntities);
                }
            }
            // 走此道路
        } else {
            // 道路不存在
            if (ConvertUtils.isEmpty(roadEntities)) {
                naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.NO_ROUTE);
                return naviExchangeEntity;
            }
            // 道路是否包含在当前路径中
            final boolean isInCurrentRoute = isInCurrentRoute(currentPathId, roadEntities);
            // 道路在当前路径中
            if (isInCurrentRoute) {
                final boolean isHasPassed = isPassed(currentPathId, roadEntities);
                // 道路已路过
                if (isHasPassed) {
                    naviExchangeEntity.setExchangeType(
                            NaviExchangeEntity.ExchangeType.ROAD_HAS_PASSED);
                } else {
                    naviExchangeEntity.setExchangeType(
                            NaviExchangeEntity.ExchangeType.ROAD_IN_CURRENT_ROUTE);
                }
                return naviExchangeEntity;
                // 道路不在当前路径中
            } else {
                return chooseOtherHasRoadPath(currentPathId, roadEntities);
            }
        }
    }

    /**
     * 选择其他路径，走道路
     * @param currentPathId 当前路径id
     * @param roadEntities 道路实体
     * @return 撤换后的路径信息
     */
    private NaviExchangeEntity chooseOtherHasRoadPath(final long currentPathId,
                                                      final ArrayList<RoadEntity> roadEntities) {
        final NaviExchangeEntity naviExchangeEntity = new NaviExchangeEntity();
        final ArrayList<NaviInfoEntity> naviInfoEntityList = mNaviAdapter.getNaviInfoList();
        NaviInfoEntity currentNaviInfoEntity = new NaviInfoEntity();
        NaviInfoEntity otherNaviInfoEntity = new NaviInfoEntity();
        long exchangePathId = -1;
        if (!ConvertUtils.isEmpty(naviInfoEntityList)) {
            for (NaviInfoEntity naviInfoEntity : naviInfoEntityList) {
                // 获取当前线路的导航信息
                if (currentPathId == naviInfoEntity.getPathId()) {
                    currentNaviInfoEntity = naviInfoEntity;
                }

                // 找到包含道路的路径
                if (!ConvertUtils.isEmpty(roadEntities)) {
                    for (RoadEntity roadEntity : roadEntities) {
                        final long naviPathId = naviInfoEntity.getPathId();
                        final long roadPathId = roadEntity.getPathId();
                        if (naviPathId == roadPathId) {
                            final boolean isHasPassed = isPassed(naviPathId, roadEntities);
                            if (isHasPassed) {
                                naviExchangeEntity.setExchangeType(NaviExchangeEntity.
                                        ExchangeType.NO_ROUTE);
                                return naviExchangeEntity;
                            } else {
                                exchangePathId = naviPathId;
                                otherNaviInfoEntity = naviInfoEntity;
                                break;
                            }
                        }
                    }
                }
            }
        }
        if (exchangePathId == -1) {
            // 没有其他路线可以选择
            naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.NO_ROUTE);
        } else {
            naviExchangeEntity.setNewRoute(getNewRoute(otherNaviInfoEntity, currentNaviInfoEntity));
            naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.NORMAL_EXCHANGE);
        }
        return naviExchangeEntity;
    }

    /**
     * 选择其他路径(未路过的情况) 不走道路
     * @param currentPathId 当前路径id
     * @param roadEntities 道路实体
     * @return 撤换后的路径信息
     */
    private NaviExchangeEntity chooseOtherNoRoadPath(final long currentPathId,
                                                     final ArrayList<RoadEntity> roadEntities) {
        final NaviExchangeEntity naviExchangeEntity = new NaviExchangeEntity();
        final ArrayList<NaviInfoEntity> naviInfoEntityList = mNaviAdapter.getNaviInfoList();
        NaviInfoEntity currentNaviInfoEntity = new NaviInfoEntity();
        NaviInfoEntity otherNaviInfoEntity = new NaviInfoEntity();
        long exchangePathId = -1;
        if (!ConvertUtils.isEmpty(naviInfoEntityList)) {
            for (NaviInfoEntity naviInfoEntity : naviInfoEntityList) {
                // 获取当前线路的导航信息
                if (currentPathId == naviInfoEntity.getPathId()) {
                    currentNaviInfoEntity = naviInfoEntity;
                }
                // 找到不包含道路的pathId
                boolean isHasOtherPath = true;
                if (!ConvertUtils.isEmpty(roadEntities)) {
                    for (RoadEntity roadEntity : roadEntities) {
                        if (naviInfoEntity.getPathId() == roadEntity.getPathId()) {
                            isHasOtherPath = false;
                            break;
                        }
                    }
                }
                if (isHasOtherPath) {
                    exchangePathId = naviInfoEntity.getPathId();
                    otherNaviInfoEntity = naviInfoEntity;
                }
            }
        }
        if (exchangePathId == -1) {
            // 没有其他路线可以选择
            naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.NO_ROUTE);
        } else {
            naviExchangeEntity.setNewRoute(getNewRoute(otherNaviInfoEntity, currentNaviInfoEntity));
            naviExchangeEntity.setExchangeType(NaviExchangeEntity.ExchangeType.NORMAL_EXCHANGE);
        }
        return naviExchangeEntity;
    }

    /**
     * @param naviInfoEntity 要切换的导航信息
     * @param currentNaviInfoEntity 当前的导航信息
     * @return 新的路径信息
     */
    private static NaviExchangeEntity.NewRoute getNewRoute(final NaviInfoEntity naviInfoEntity,
                                                           final NaviInfoEntity
                                                                   currentNaviInfoEntity) {
        final NaviExchangeEntity.NewRoute newRoute = new NaviExchangeEntity.
                NewRoute();
        newRoute.setComePareTime(naviInfoEntity.getRemainTime() -
                currentNaviInfoEntity.getRemainTime());
        newRoute.setComePareTrafficLights(naviInfoEntity.
                getRouteRemainLightCount() -
                currentNaviInfoEntity.getRouteRemainLightCount());
        newRoute.setPathId(naviInfoEntity.getPathId());
        if (Logger.openLog) {
            Logger.i(TAG, "getNewRoute newRoute:" + newRoute.toString());
        }
        return newRoute;
    }

    /**
     * @param currentPathId 当前路径id
     * @param roadEntities 道路实体
     * @return true:在当前路径中 false:不在当前路径中
     */
    private boolean isInCurrentRoute(final long currentPathId,
                                     final ArrayList<RoadEntity> roadEntities) {
        boolean isInCurrentRoute = false;
        for (RoadEntity roadEntity : roadEntities) {
            if (roadEntity.getPathId() == currentPathId) {
                isInCurrentRoute = true;
                break;
            }
        }
        return isInCurrentRoute;
    }

    /**
     * @param currentPathId 当前路径id
     * @param roadEntities 道路实体
     * @return 是否已经通过 true:已经通过 false:未通过
     */
    private boolean isPassed(final long currentPathId, final ArrayList<RoadEntity> roadEntities) {
        final List<NaviInfoEntity> naviInfoEntityList = mNaviAdapter.getNaviInfoList();
        long curSegIdx = -1;
        long roadSegmentIdx = -1;
        if (!ConvertUtils.isEmpty(naviInfoEntityList)) {
            for (NaviInfoEntity naviInfoEntity : naviInfoEntityList) {
                if (currentPathId == naviInfoEntity.getPathId()) {
                    curSegIdx = naviInfoEntity.getCurSegIdx();
                }
            }
        }
        if (!ConvertUtils.isEmpty(naviInfoEntityList)) {
            for (RoadEntity roadEntity : roadEntities) {
                if (currentPathId == roadEntity.getPathId()) {
                    roadSegmentIdx = roadEntity.getSegIdx();
                }
            }
        }
        Logger.i(TAG, "isPassed curSegIdx:" + curSegIdx + " roadSegmentIdx:" +
                roadSegmentIdx);
        return curSegIdx >= roadSegmentIdx;
    }


    /**
     * 获取道路在所有导航路径中的路段索引
     * @param roadName 道路名称
     * @return segIdx
     */
    private ArrayList<RoadEntity> getRoadSegmentIdx(final String roadName) {
        final List<PathInfo> pathList = OpenApiHelper.getCurrentPathInfos();
        final ArrayList<RoadEntity> roadEntities = new ArrayList<>();
        if (!ConvertUtils.isEmpty(pathList)) {
            for (PathInfo pathInfo : pathList) {
                final long segmentCount = pathInfo.getSegmentCount();
                for (long i = 0; i < segmentCount; i++) {
                    final SegmentInfo segment = pathInfo.getSegmentInfo(i);
                    final long linkCount = segment.getLinkCount();
                    for (long j = 0; j < linkCount; j++) {
                        final LinkInfo link = segment.getLinkInfo(j);
                        final String linkName = link.getRoadName();
                        final long curPathId = pathInfo.getPathID();
                        if (roadName.equals(linkName)) {
                            if (isCanAddRoadEntity(curPathId, i, roadEntities)) {
                                final RoadEntity roadEntity = new RoadEntity();
                                roadEntity.setPathId(curPathId);
                                roadEntity.setSegIdx(i);
                                roadEntities.add(roadEntity);
                            }
                        }
                    }
                }
            }
        }
        if (Logger.openLog) {
            Logger.i(TAG, "getRoadSegmentIdx roadEntities:" + roadEntities.toString());
        }
        return roadEntities;
    }

    /**
     * 因为一个路段里面可能也包含其他的路段，所以只添加路段索引最小也就是离车最近的路段信息
     * @param pathId  路径id
     * @param segIdx 路段索引
     * @param roadEntities 道路实体
     * @return 是否可以添加道路实体 true:可以添加 false:不可以添加
     */
    private boolean isCanAddRoadEntity(final long pathId, final long segIdx,
                                       final ArrayList<RoadEntity> roadEntities) {
        if (ConvertUtils.isEmpty(roadEntities)) {
            return true;
        }
        for (RoadEntity roadEntity : roadEntities) {
            if (pathId == roadEntity.mPathId && roadEntity.getSegIdx() <= segIdx) {
                return false;
            }
        }
        return true;
    }

    public static class RoadEntity {
        // 包含道路的路径ID
        private long mPathId;
        // 路段索引
        private long mSegIdx;

        public RoadEntity() {
        }

        public RoadEntity(final long pathId, final int segIdx) {
            this.mPathId = pathId;
            this.mSegIdx = segIdx;
        }

        public long getPathId() {
            return mPathId;
        }

        public void setPathId(final long pathId) {
            this.mPathId = pathId;
        }

        public long getSegIdx() {
            return mSegIdx;
        }

        public void setSegIdx(final long segIdx) {
            this.mSegIdx = segIdx;
        }

        @NonNull
        @Override
        public String toString() {
            return "RoadEntity{" +
                    "pathId=" + mPathId +
                    ", segIdx=" + mSegIdx +
                    '}';
        }
    }
}
