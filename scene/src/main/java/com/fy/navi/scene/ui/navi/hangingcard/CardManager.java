package com.fy.navi.scene.ui.navi.hangingcard;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.scene.util.HandCardType;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/19
 * Description: [悬挂卡帮助类]
 */
public class CardManager {
    private CardManager() {
    }

    private static final class InstanceHolder {
        private static final CardManager instance = new CardManager();
    }

    public static CardManager getInstance() {
        return InstanceHolder.instance;
    }


    /***
     * UE-2.6-1
     * -充电位紧张：总充电位数<=30个，剩余充电位<30% ；总充电位数>30个，剩余充电位<10% 或 剩余充电位少于10个。
     * @return true 紧张 false 充足
     */
    public boolean isPlentiful(final ChargeInfo chargeInfo) {
        final int totalSize = chargeInfo.getFast_total() + chargeInfo.getSlow_total();
        final int freeTotal = chargeInfo.getFast_free() + chargeInfo.getSlow_free();
        return totalSize <= 30 || (totalSize > 30 && freeTotal < 10);
    }

    /***
     * 判断距离是否满足预期
     * @param distance
     * @param distanceCondition
     * @return
     */
    public boolean isEligible(int distance, int distanceCondition) {
        return distance <= distanceCondition;
    }

    /***
     *
     * @param searchResultEntity
     * @param endPoiInfo
     * @return
     */
    public ArrayList<PoiInfoEntity> getParkList(final SearchResultEntity searchResultEntity, final PoiInfoEntity endPoiInfo) {
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        final ArrayList<PoiInfoEntity> tmpList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(searchResultEntity.getPoiList()) || !ConvertUtils.isNull(endPoiInfo)) {
            tmpList.addAll(searchResultEntity.getPoiList());
            // 按照距离排序
            tmpList.sort((o1, o2) -> getRealDis(o1) >= getRealDis(o2) ? 0 : 1);

            // -终点是停车场但停车位紧张导致的推荐，【当前终点】放在列表第二位。 UE:1.12-2
            if (endIsParking(endPoiInfo)) {
                if (ConvertUtils.isEmpty(tmpList)) {
                    dataList.add(endPoiInfo);
                } else if (tmpList.size() == 1) {
                    dataList.addAll(tmpList);
                    dataList.add(endPoiInfo);
                } else {
                    for (int i = 0; i < tmpList.size(); i++) {
                        if (i == 1) {
                            dataList.add(endPoiInfo);
                        }
                        dataList.add(tmpList.get(i));
                    }
                }
            } else {
                dataList.addAll(tmpList);
            }
        }
        ArrayList<PoiInfoEntity> finalData = new ArrayList<>();
        finalData.addAll(dataList.size() > 3 ? dataList.subList(0, 3) : dataList);
        return finalData;
    }

    private double getRealDis(PoiInfoEntity poiInfo) {
        final String dis = poiInfo.getDistance();
        double tmp;
        try {
            if (ConvertUtils.isEmpty(dis)) {
                tmp = 0;
            } else if (dis.contains("千米") || dis.contains("公里")) {
                dis.replaceAll("千米", "");
                dis.replaceAll("公里", "");
                tmp = Double.parseDouble(dis) * 1000;
            } else if (dis.contains("米")) {
                dis.replaceAll("米", "");
                tmp = Double.parseDouble(dis);
            } else {
                tmp = 0;
            }
        } catch (Exception e) {
            tmp = 0;
        }
        return tmp;
    }

    /***
     * 判断终点是否是停车场
     * @return
     */
    public boolean endIsParking(final PoiInfoEntity poiInfo) {
        final boolean result = !ConvertUtils.isNull(poiInfo)
                && !ConvertUtils.isEmpty(poiInfo.getPoiTag())
                && poiInfo.getPoiTag().contains(AppContext.getInstance().getMApplication().getString(com.fy.navi.scene.R.string.st_quick_search_parking));
        return result;
    }

    /***
     *
     * @param searchResultEntity
     * @return
     */
    public ArrayList<PoiInfoEntity> getStationList(final SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isNull(searchResultEntity) || ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            return new ArrayList<>();
        } else {
            return (ArrayList<PoiInfoEntity>) searchResultEntity.getPoiList();
        }
    }

    /***
     * 判断停车场是否紧张
     * -停车位紧张：总车位数<=30个，剩余车位<30% ；总车位数>30个，剩余车位<10% 或 剩余车位少于10个。
     */
    public boolean parkIsCrowed(final PoiInfoEntity poiInfo) {
        if (ConvertUtils.isNull(poiInfo) || ConvertUtils.isEmpty(poiInfo.getParkingInfoList())) {
            return false;
        }
        final ParkingInfo parkingInfo = poiInfo.getParkingInfoList().get(0);
        final int totalSize = parkingInfo.getSpaceTotal();
        final int spaceSize = parkingInfo.getSpaceFree();
        if (totalSize <= 0) return false;
        return (totalSize <= 30 && spaceSize * 1f / totalSize < 0.3) || (totalSize > 30 && (spaceSize * 1f / totalSize < 0.1 || spaceSize < 10));
    }

    public boolean getSceneOnShow(final NaviSceneId sceneId) {
        final NaviSceneBase sceneBase = NaviSceneManager.getInstance().getSceneById(sceneId);
        if (ConvertUtils.isNull(sceneBase)) {
            return false;
        } else {
            return sceneBase.isVisible();
        }
    }

    public CardView createCardViewByType(NaviSceneHangingCard hangingCard, HandCardType type) {
        final Context context = hangingCard.getContext();
        return switch (type) {
            case CHARGE ->
                    new ChargeCardView(context, hangingCard.getImpl(), hangingCard.getImpl().getData(type), type);
            case GAS ->
                    new GasCardView(context, hangingCard.getImpl(), hangingCard.getImpl().getData(type), type);
            case PARK ->
                    new ParkCardView(context, hangingCard.getImpl(), hangingCard.getImpl().getData(type), type);
            default -> {
                throw new UnknownError("未知类型！");
            }
        };
    }
}
