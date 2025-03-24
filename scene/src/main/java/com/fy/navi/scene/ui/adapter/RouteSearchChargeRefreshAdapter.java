package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSearchChargeRefreshListItemBinding;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class RouteSearchChargeRefreshAdapter extends RecyclerView.Adapter<RouteSearchChargeRefreshAdapter.Holder> {
    private List<PoiInfoEntity> mRouteBeanList;
    private List<RouteParam> mLoaclSaveEntity;
    private int mSearchType;
    private OnItemClickListener mItemClickListener;
    public RouteSearchChargeRefreshAdapter() {
        mRouteBeanList = new ArrayList<>();
        mLoaclSaveEntity = new ArrayList<>();
    }
    /***
     * 展示充电站列表
     * @param routeBeanList 搜索数据
     * @param loaclSaveEntity 本地已添加数据
     * @param searchType 搜索方式 0-沿途搜索
     */
    public void setRouteBeanList(final List<PoiInfoEntity> routeBeanList, final List<RouteParam> loaclSaveEntity, final int searchType) {
        if (null == routeBeanList) {
            return;
        }
        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        mLoaclSaveEntity.clear();
        mLoaclSaveEntity.addAll(loaclSaveEntity);
        mSearchType = searchType;
        notifyDataSetChanged();
    }
    /**
     * 设置监听
     * @param itemClickListener 监听
     * */
    public void setmItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(final ViewGroup parent, final int viewType) {
        final RouteSearchChargeRefreshListItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_search_charge_refresh_list_item, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteBeanList == null) {
            return 0;
        }
        return mRouteBeanList.size();
    }

    @Override
    public void onBindViewHolder(final Holder holder, final int position) {
        holder.mRouteSearchChargeRefreshListItemBinding.itemRootViewCharge.setVisibility(ViewGroup.VISIBLE);
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeNum.setText("" + (position + NumberUtils.NUM_1));
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeName.setText(mRouteBeanList.get(position).getName());
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeDescription.setText(
                mRouteBeanList.get(position).getDistance() + " | " + mRouteBeanList.get(position).getAddress());
        final boolean belongRouteParam;
        if (mSearchType == 0) {
            belongRouteParam = isBelongSamePoi(mLoaclSaveEntity, mRouteBeanList.get(position));
        } else {
            belongRouteParam = RoutePackage.getInstance().isBelongRouteParam(MapTypeId.MAIN_SCREEN_MAIN_MAP, mRouteBeanList.get(position));
        }
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeAddText.setText(belongRouteParam
                ? ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_added)
                : ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_add));
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeAddImg.setImageDrawable(
                belongRouteParam ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added)
                        : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
        if (mRouteBeanList.get(position).getChargeInfoList() != null && mRouteBeanList.get(position).getChargeInfoList().size() != 0) {
            final ChargeInfo chargeInfo = mRouteBeanList.get(position).getChargeInfoList().get(0);
            final String fastFree = chargeInfo.getFast_free() == 0 ? "--" : chargeInfo.getFast_free() + "";
            final String fastTotal = chargeInfo.getFast_total() == 0 ? "" : "/" + chargeInfo.getFast_total();
            holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeFastNumOne.setText(fastFree);
            if (!ConvertUtils.isEmpty(fastTotal)) {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeFastNumTwo.setText(fastTotal);
            } else {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeFastNumTwo.setVisibility(View.GONE);
            }

            final String slowFree = chargeInfo.getSlow_free() == 0 ? "--" : chargeInfo.getSlow_free() + "";
            final String slowTotal = chargeInfo.getSlow_total() == 0 ? "" : "/" + chargeInfo.getSlow_total();
            holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeLowNumOne.setText(slowFree);
            if (!ConvertUtils.isEmpty(slowTotal)) {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeLowNumTwo.setText(fastTotal);
            } else {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeLowNumTwo.setVisibility(View.GONE);
            }
            if (!ConvertUtils.isEmpty(mRouteBeanList.get(position).getChargeInfoList().get(0).getCurrentElePrice())) {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargePriceName.setVisibility(View.VISIBLE);
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setVisibility(View.VISIBLE);
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setText(
                        chargeInfo.getCurrentElePrice() + "/度");
            } else {
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargePriceNum.setVisibility(View.GONE);
                holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargePriceName.setVisibility(View.GONE);
            }
        }
        holder.mRouteSearchChargeRefreshListItemBinding.itemRootViewCharge.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(mRouteBeanList.get(position));
            }
        });

        final boolean finalBelongRouteParam = belongRouteParam;
        holder.mRouteSearchChargeRefreshListItemBinding.routeItemChargeAddBg.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                if (finalBelongRouteParam) {
                    mItemClickListener.onItermRemoveClick(mRouteBeanList.get(position));
                } else {
                    mItemClickListener.onItermAddClick(mRouteBeanList.get(position));
                }

            }
        });
    }
    /**
     * 是否在路线上
     * @param loaclSaveEntity 路线上的点
     * @param poiInfoEntity 当前点
     * @return  itemClickListener 监听
     * */
    private boolean isBelongSamePoi(final List<RouteParam> loaclSaveEntity, final PoiInfoEntity poiInfoEntity) {
        if (loaclSaveEntity.isEmpty()) {
            return false;
        }
        for (RouteParam param: loaclSaveEntity) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSearchChargeRefreshListItemBinding mRouteSearchChargeRefreshListItemBinding;

        public Holder(final RouteSearchChargeRefreshListItemBinding routeSearchChargeRefreshListItemBinding) {
            super(routeSearchChargeRefreshListItemBinding.getRoot());
            this.mRouteSearchChargeRefreshListItemBinding = routeSearchChargeRefreshListItemBinding;
            routeSearchChargeRefreshListItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /**
         * item点击
         * @param poiInfoEntity 当前点
         * */
        void onItemClick(final PoiInfoEntity poiInfoEntity);
        /**
         * 移除点
         * @param poiInfoEntity 当前点
         * */
        void onItermRemoveClick(final PoiInfoEntity poiInfoEntity);
        /**
         * 添加点
         * @param poiInfoEntity 当前点
         * */
        void onItermAddClick(final PoiInfoEntity poiInfoEntity);
    }
}