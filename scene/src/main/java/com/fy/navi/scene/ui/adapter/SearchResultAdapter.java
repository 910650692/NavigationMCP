
package com.fy.navi.scene.ui.adapter;


import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.databinding.SearchResultItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class SearchResultAdapter extends RecyclerView.Adapter<SearchResultAdapter.ResultHolder> {
    private SearchResultEntity mSearchResultEntity;
    private final List<PoiInfoEntity> mPoiEntities;
    private OnItemClickListener mOnItemClickListener;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private PoiInfoEntity mPoiInfoEntity;
    private int mHomeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
    private final int mSpanCount = 2;
    private final int mSpacing = 16;
    private final int mHorizontalSpacing = 16;
    private PoiInfoEntity mChildSelectInfo;
    private int mChildSelectIndex = -1;
    private int mParentSelectIndex;
    private int mLastParentSelectIndex;
    private boolean isFirstRefresh = true;
    public void setOnItemClickListener(final OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    public int getHomeCompanyType() {
        return mHomeCompanyType;
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }

    public SearchResultAdapter() {
        this.mPoiEntities = new ArrayList<>();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    /**
     * 清空列表
     */
    public void clearList() {
        final int size = mPoiEntities.size();
        mPoiEntities.clear();
        if (size > 0) {
            notifyItemRangeRemoved(0, size);
        }
    }

    /**
     * 刷新列表
     * @param searchResultEntity 源数据
     */
    public void notifyList(final SearchResultEntity searchResultEntity) {
        this.mSearchResultEntity = searchResultEntity;
        final List<PoiInfoEntity> newPoiList = searchResultEntity.getPoiList();
        final int oldSize = mPoiEntities.size();
        final int newSize = newPoiList.size();

        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (allPoiParamList.size() >= 2) {
            allPoiParamList.remove(0);
            allPoiParamList.remove(allPoiParamList.size() - 1);
        }

        mPoiEntities.clear();
        mPoiEntities.addAll(newPoiList);
        isFirstRefresh = true;
        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final SearchResultItemBinding adapterSearchResultItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.search_result_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        holder.mResultItemBinding.setPoiBean(mPoiEntities.get(position));
        mPoiInfoEntity = mPoiEntities.get(position);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onBindViewHolder searchType1: "
                + mSearchResultEntity.getSearchType() + "----mHomeCompanyType: "+mHomeCompanyType);
        if (mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            holder.mResultItemBinding.poiNum.setVisibility(View.VISIBLE);
            holder.mResultItemBinding.poiIcon.setVisibility(View.GONE);
            holder.mResultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        } else {
            holder.mResultItemBinding.poiNum.setVisibility(View.GONE);
            holder.mResultItemBinding.poiIcon.setVisibility(View.VISIBLE);
        }
        if (mPoiInfoEntity != null && ConvertUtils.isEmpty(mPoiInfoEntity.getAddress())) {
            holder.mResultItemBinding.subLineView.setVisibility(View.GONE);
        }
        //预搜索高亮搜索文本
        if (mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            holder.mResultItemBinding.poiName.setText(matcherSearchTitle(AppContext.getInstance().
                    getMContext().getResources().getColor(R.color.navi_color_006CFF_100),
                    mPoiEntities.get(position).getName(), mSearchResultEntity.getKeyword()));
        }
        if (mSearchPackage.isAlongWaySearch()) {
            holder.mResultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add));
            if (mRoutePackage.isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, mPoiEntities.get(position))) {
                holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                        ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added));
            } else {
                holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_add);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                        ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
            }
        } else {
            holder.mResultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

        }
        if (mHomeCompanyType != -1) {
            holder.mResultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
            switch (mHomeCompanyType) {
                case 0:
                case 3:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_collect_add);
                    break;
                case 1:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_home);
                    break;
                case 2:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_company);
                    break;
                default:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_go_here);
                    break;
            }
        }
        setOnClickListener(holder, position);
        if (mPoiInfoEntity != null) {
            if ("0米".equals(mPoiInfoEntity.getDistance())) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mPoiInfoEntity.getDistance() is: " + mSearchPackage.calcStraightDistance(mPoiInfoEntity.getPoint()));
               mPoiInfoEntity.setDistance(mSearchPackage.calcStraightDistance(mPoiInfoEntity.getPoint()));
            }
            if (mPoiInfoEntity.getPointTypeCode() == null) {
                refreshNormalView(holder);
                return;
            }
            final int pointTypeCode = mSearchPackage.getPointTypeCode(mPoiInfoEntity.getPointTypeCode());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
            switch (pointTypeCode) {
                case AutoMapConstant.PointTypeCode.GAS_STATION:
                    refreshGasStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                    refreshChargeStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CAR_WASH:
                    refreshCarWashView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CATERING:
                    refreshCateringView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.PARKING_LOT:
                    refreshParkingLotView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                    refreshServiceAreaView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                case AutoMapConstant.PointTypeCode.OTHERS:
                default:
                    refreshScenicSpotView(holder);
                    break;
            }
        }
    }

    /**
     * 设置点击事件
     * @param holder holder
     * @param position 下标
     */
    private void setOnClickListener(@NonNull final ResultHolder holder, final int position) {
        holder.mResultItemBinding.getRoot().setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 详情");
            if (mOnItemClickListener != null) {
                if (mParentSelectIndex == position && mChildSelectIndex != -1) {
                    mOnItemClickListener.onChildItemClick(position, mPoiEntities.get(position), mChildSelectIndex);
                } else {
                    mOnItemClickListener.onItemClick(position, mPoiEntities.get(position));
                }
            }
        });

        holder.mResultItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 算路|添加途经点");
            if (mOnItemClickListener != null) {
                if (mParentSelectIndex == position && mChildSelectIndex != -1 && mChildSelectInfo != null) {
                    mOnItemClickListener.onNaviClick(position, mChildSelectInfo);
                } else {
                    mOnItemClickListener.onNaviClick(position, mPoiEntities.get(position));
                }
            }
        });
    }

    /**
     * 刷新通用视图
     * @param resultHolder holder
     */
    private void refreshNormalView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshNormalView do nothing");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(View.GONE);
    }

    /**
     * 刷新景点视图
     * @param resultHolder holder
     */
    private void refreshScenicSpotView(final ResultHolder resultHolder) {
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPrice.setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPriceIcon.setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.getRoot().setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setVisibility(View.GONE);
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setAdapter(null);
        if (mPoiInfoEntity.getAverageCost() == -1 || mPoiInfoEntity.getAverageCost() == 0) {
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPrice.setVisibility(View.GONE);
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPriceIcon.setVisibility(View.GONE);
        } else {
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotPrice.setText(String.valueOf(mPoiInfoEntity.getAverageCost()));
        }

        final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
        final PoiDetailsScenicChildAdapter scenicChildAdapter = new PoiDetailsScenicChildAdapter();
        if (childInfoList != null && !childInfoList.isEmpty()) {
            scenicChildAdapter.setChildInfoList(childInfoList);
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setVisibility(View.VISIBLE);
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setLayoutManager(
                    new GridLayoutManager(resultHolder.mResultItemBinding.getRoot().getContext(), mSpanCount));
            if (resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.getItemDecorationCount() == 0) {
                resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.addItemDecoration(
                        new GridSpacingItemDecoration(resultHolder.mResultItemBinding.getRoot().getContext(), mSpanCount, mSpacing, mSpacing, false));
            }
            resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.poiScenicSpotChildList.setAdapter(scenicChildAdapter);
            scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                mChildSelectIndex = index;
                if (isFirstRefresh) {
                    mLastParentSelectIndex = resultHolder.getAdapterPosition();
                    isFirstRefresh = false;
                }
                mParentSelectIndex = resultHolder.getAdapterPosition();
                if (mLastParentSelectIndex != mParentSelectIndex) {
                    if (!ConvertUtils.isEmpty(mPoiEntities) && mLastParentSelectIndex < mPoiEntities.size()) {
                        final PoiInfoEntity poiInfoEntity = mPoiEntities.get(mLastParentSelectIndex);
                        if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(poiInfoEntity.getChildInfoList())) {
                            for (ChildInfo childInfo : poiInfoEntity.getChildInfoList()) {
                                childInfo.setChecked(0);
                            }
                        }
                        notifyItemChanged(mLastParentSelectIndex);
                    }
                }
                final ChildInfo childInfo = childInfoList.get(index);
                mChildSelectInfo = new PoiInfoEntity()
                        .setName(childInfo.getName())
                        .setAddress(childInfo.getAddress())
                        .setPid(childInfo.getPoiId())
                        .setPoint(childInfo.getLocation());
                mLastParentSelectIndex = resultHolder.getAdapterPosition();
            });
        }
        resultHolder.mResultItemBinding.scenePoiItemScenicSpotView.getRoot().setVisibility(View.VISIBLE);
    }

    /**
     * 刷新服务区视图
     * @param resultHolder holder
     */
    private void refreshServiceAreaView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshServiceAreaView");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(View.GONE);
    }

    /***
     * 判断当前点是否已经在路线上
     * @param local 路径上的点
     * @param poiInfoEntity 当前点
     * @return 是否是属于路径上的点
     */
    private boolean isBelongSamePoi(final List<RouteParam> local, final PoiInfoEntity poiInfoEntity) {
        if (local.isEmpty()) {
            return false;
        }
        for (RouteParam param : local) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /***
     * 从添加的点中删除poi点
     * @param local 路径上的点
     * @param poiInfoEntity 当前点
     */
    private void removeSamePoi(final List<RouteParam> local, final PoiInfoEntity poiInfoEntity) {
        if (local.isEmpty()) {
            return;
        }
        for (RouteParam param : local) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                local.remove(param);
                break;
            }
        }
    }

    /**
     * 刷新停车场视图
     * @param resultHolder holder
     */
    private void refreshParkingLotView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshParkingLotView");
        if (mPoiInfoEntity == null || mPoiInfoEntity.getParkingInfoList() == null || mPoiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.poiParkingFreeTotal.setText("");
        resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.VISIBLE);
        final ParkingInfo parkingInfo = mPoiInfoEntity.getParkingInfoList().get(0);
        String parkString = "";
        final int spaceFree = parkingInfo.getSpaceFree();
        final int spaceTotal = parkingInfo.getSpaceTotal();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
        if (spaceFree == -1 && spaceTotal == -1) {
            resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.GONE);
        } else if (spaceFree == -1) {
            parkString = resultHolder.mResultItemBinding.getRoot().getContext().getString(R.string.parking_lot_total, spaceTotal);
            resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.VISIBLE);
            resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.poiParkingFreeTotal.setText(parkString);
        } else {
            resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.getRoot().setVisibility(View.VISIBLE);
            parkString = resultHolder.mResultItemBinding.getRoot().getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
            resultHolder.mResultItemBinding.scenePoiItemParkingServiceView.poiParkingFreeTotal.setText(parkString);
        }
    }

    /**
     * 刷新餐饮视图
     * @param resultHolder holder
     */
    private void refreshCateringView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshCateringView" + mPoiInfoEntity.getAverageCost());
        resultHolder.mResultItemBinding.scenePoiItemCateringView.getRoot().setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemCateringView.poiCateringPrice.setText("");
        if (mPoiInfoEntity.getAverageCost() == -1) {
            resultHolder.mResultItemBinding.scenePoiItemCateringView.getRoot().setVisibility(View.GONE);
        } else {
            resultHolder.mResultItemBinding.scenePoiItemCateringView.poiCateringPrice.setText(String.valueOf(mPoiInfoEntity.getAverageCost()));
            resultHolder.mResultItemBinding.scenePoiItemCateringView.getRoot().setVisibility(View.VISIBLE);
        }
    }

    /**
     * 刷新洗车视图
     * @param resultHolder holder
     */
    private void refreshCarWashView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshCarWashView");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(View.GONE);
    }

    /**
     * 刷新充电站视图
     * @param resultHolder holder
     */
    private void refreshChargeStationView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshChargeStationView");
        // 重置includeview中视图状态避免数据重用导致数据异常加载
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastRoot.setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowRoot.setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastFree.setText("");
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setText("");
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowFree.setText("");
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setText("");
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setVisibility(View.VISIBLE);

        final List<ChargeInfo> chargeInfos = mPoiInfoEntity.getChargeInfoList();
        final ChargeInfo chargeInfo = chargeInfos.get(0);
        final String fastFree = chargeInfo.getFast_free() == 0 ? "" : chargeInfo.getFast_free() + "";
        String fastTotal = chargeInfo.getFast_total() == 0 ? "" : "/" + chargeInfo.getFast_total();
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastFree.setText(fastFree);
        if (ConvertUtils.isEmpty(fastFree) && ConvertUtils.isEmpty(fastTotal)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastRoot.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastFree)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastFree.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastTotal)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setVisibility(View.GONE);
        }
        if (!ConvertUtils.isEmpty(fastTotal)) {
            if (ConvertUtils.isEmpty(fastFree)) {
                fastTotal = chargeInfo.getFast_total() + "";
            }
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeFastTotal.setText(fastTotal);
        }

        final String slowFree = chargeInfo.getSlow_free() == 0 ? "" : chargeInfo.getSlow_free() + "";
        String slowTotal = chargeInfo.getSlow_total() == 0 ? "" : "/" + chargeInfo.getSlow_total();
        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowFree.setText(slowFree);
        if (ConvertUtils.isEmpty(slowFree) && ConvertUtils.isEmpty(slowTotal)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowRoot.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(slowFree)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowFree.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(slowTotal)) {
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setVisibility(View.GONE);
        }
        if (!ConvertUtils.isEmpty(slowTotal)) {
            if (ConvertUtils.isEmpty(slowFree)) {
                slowTotal = chargeInfo.getSlow_total() + "";
            }
            resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargeSlowTotal.setText(slowTotal);
        }

        resultHolder.mResultItemBinding.scenePoiItemChargeView.poiChargePrice.setText(
                resultHolder.mResultItemBinding.getRoot().getContext().getString(
                R.string.charge_price_simple, chargeInfo.getCurrentElePrice()));
        resultHolder.mResultItemBinding.scenePoiItemChargeView.getRoot().setVisibility(View.VISIBLE);

    }

    /**
     * 刷新加油站视图
     * @param resultHolder holder
     */
    private void refreshGasStationView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshGasStationView");
        final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            if (!gasStationInfo.getPrice().contains("升")) {
                gasStationInfo.setPrice(resultHolder.mResultItemBinding.getRoot().getContext().
                        getString(R.string.oil_price, gasStationInfo.getPrice()));
            }
        }
        final GasStationAdapter gasStationAdapter = new GasStationAdapter();
        //根据UE只显示前两个油品种类
        if (!ConvertUtils.isEmpty(gasStationInfos) && gasStationInfos.size() > 2) {
            // 截取前两个元素
            final List<GasStationInfo> subList = gasStationInfos.subList(0, 2);
            // 使用ArrayList构造函数创建新的列表
            final ArrayList<GasStationInfo> newList = new ArrayList<>(subList);
            gasStationAdapter.setGasStationList(newList);
        } else {
            gasStationAdapter.setGasStationList(gasStationInfos);
        }
        resultHolder.mResultItemBinding.scenePoiItemGasView.getRoot().setVisibility(View.VISIBLE);
        resultHolder.mResultItemBinding.scenePoiItemGasView.poiGasOilList.setLayoutManager(
                new GridLayoutManager(resultHolder.mResultItemBinding.getRoot().getContext(), mSpanCount));
        resultHolder.mResultItemBinding.scenePoiItemGasView.poiGasOilList.setAdapter(gasStationAdapter);

    }


    @Override
    public int getItemCount() {
        return mPoiEntities.size();
    }


    /**
     * 清除途径点列表
     */
    public void clearViaList() {
        notifyDataSetChanged();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
         private final SearchResultItemBinding mResultItemBinding;

        public ResultHolder(final SearchResultItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.mResultItemBinding = resultItemBinding;
            this.mResultItemBinding.setHolder(this);
        }
    }

    /**
     * 高亮显示文本
     *
     * @param color   需要高亮显示的颜色
     * @param text    显示的文字
     * @param keyword 搜索关键词
     * @return SpannableString
     */
    private SpannableString matcherSearchTitle(final int color, final String text, final String keyword) {
        final SpannableString spannableString = new SpannableString(text);
        if (null != keyword && !TextUtils.isEmpty(keyword)) {
            try {
                final Pattern pattern = Pattern.compile(keyword);
                final Matcher matcher = pattern.matcher(spannableString);
                while (matcher.find()) {
                    final int start = matcher.start();
                    final int end = matcher.end();
                    spannableString.setSpan(new ForegroundColorSpan(color), start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
                }
            } catch (PatternSyntaxException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Invalid regex pattern: " + e.getMessage());
            } catch (IndexOutOfBoundsException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Index out of bounds: " + e.getMessage());
            } catch (IllegalStateException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Matcher state error: " + e.getMessage());
            } catch (IllegalArgumentException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Argument error: " + e.getMessage());
            }
        }
        return spannableString;
    }

    public interface OnItemClickListener {
        /**
         * 点击事件
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         */
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         * 导航按钮点击事件
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         */
        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         *
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         * @param childPosition 选中的子点下标
         */
        default void onChildItemClick(int position, PoiInfoEntity poiInfoEntity, int childPosition){

        }
    }
}