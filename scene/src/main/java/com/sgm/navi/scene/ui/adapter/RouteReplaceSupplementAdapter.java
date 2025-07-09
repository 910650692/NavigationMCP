package com.sgm.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteReplaceSupplementItemBinding;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;


public class RouteReplaceSupplementAdapter extends RecyclerView.Adapter<RouteReplaceSupplementAdapter.Holder>{
    private ArrayList<RouteAlterChargeStationInfo> mRouteAlterChargeStationInfo;
    private ArrayList<PoiInfoEntity> mPoiInfoEntities;
    private OnItemClickListener mItemClickListener;

    public RouteReplaceSupplementAdapter() {
        mRouteAlterChargeStationInfo = new ArrayList<>();
        mPoiInfoEntities = new ArrayList<>();
    }

    /***
     * 设置可替换补能点显示
     * @param routeAlterChargeStationInfos 子节点列表
     */
    public void setRouteSupplementInfos(final ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
        if (null == routeAlterChargeStationInfos) {
            return;
        }
        mRouteAlterChargeStationInfo.clear();
        mRouteAlterChargeStationInfo.addAll(routeAlterChargeStationInfos);
    }

    /***
     * 设置可替换补能点详情数据
     * @param poiInfoEntities 详情列表
     */
    public void setPoiInfoEntities(final ArrayList<PoiInfoEntity> poiInfoEntities) {
        ThreadManager.getInstance().postUi(() -> {
            if (null == poiInfoEntities) {
                return;
            }
            mPoiInfoEntities.clear();
            mPoiInfoEntities.addAll(poiInfoEntities);
            notifyDataSetChanged();
        });
    }

    public void setItemClickListener(OnItemClickListener mItemClickListener) {
        this.mItemClickListener = mItemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteReplaceSupplementItemBinding routeReplaceSupplementItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_replace_supplement_item, parent, false);
        return new RouteReplaceSupplementAdapter.Holder(routeReplaceSupplementItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointIndex.setText(String.format(
                AppCache.getInstance().getMContext().getResources().getString(R.string.route_alternative),
                position + 1));
        if (position < mPoiInfoEntities.size()) {
            PoiInfoEntity poiInfoEntity = mPoiInfoEntities.get(position);
            setPoiInfoEntityView(holder, poiInfoEntity);
            holder.mRouteReplaceSupplementItemBinding.btReplace.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (mItemClickListener != null) {
                        mItemClickListener.onItemClick(poiInfoEntity);
                    }
                }
            });
        }

        if (position < mRouteAlterChargeStationInfo.size()) {
            RouteAlterChargeStationInfo routeAlterChargeStationInfo = mRouteAlterChargeStationInfo.get(position);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointDistance.setText(TimeUtils.getInstance()
                    .getDistanceString(routeAlterChargeStationInfo.getMDistance()));
        }

    }

    @Override
    public int getItemCount() {
        return mPoiInfoEntities.size();
    }

    /**
     * 返回数据
     *
     * @param holder holder
     * @param poiInfoEntity POI详情参数
     */
    public void setPoiInfoEntityView(@NonNull final Holder holder, final PoiInfoEntity poiInfoEntity) {
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointName.setText(poiInfoEntity.getMName());
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointAddress.setText(poiInfoEntity.getAddress());
        final ChargeInfo chargeInfo = poiInfoEntity.getChargeInfoList().get(0);
        final String fastFree = chargeInfo.getFast_free() == -1 ? "" : String.valueOf(chargeInfo.getFast_free());
        final String fastTotal = chargeInfo.getFast_total() == -1 ? "" : "/" + chargeInfo.getFast_total();
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastCurrent.setText(fastFree);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTotal.setText(fastTotal);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTotal.setVisibility(View.VISIBLE);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastCurrent.setVisibility(View.VISIBLE);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTitle.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(fastFree) && ConvertUtils.isEmpty(fastTotal)) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTotal.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastCurrent.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTitle.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastFree)) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastCurrent.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointFastTotal.setText(fastTotal.substring(1));
        }

        final String slowFree = chargeInfo.getSlow_free() == -1 ? "" : String.valueOf(chargeInfo.getSlow_free());
        final String slowTotal = chargeInfo.getSlow_total() == -1 ? "" : "/" + chargeInfo.getSlow_total();
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowCurrent.setText(slowFree);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTotal.setText(slowTotal);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTotal.setVisibility(View.VISIBLE);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowCurrent.setVisibility(View.VISIBLE);
        holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTitle.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(slowFree) && ConvertUtils.isEmpty(slowTotal)) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTotal.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowCurrent.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTitle.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastFree)) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowCurrent.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSlowTotal.setText(slowTotal.substring(1));
        }

        if (chargeInfo.getCurrentElePrice().startsWith("-")) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSpend.setVisibility(View.GONE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSpendImage.setVisibility(View.GONE);
        } else {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSpend.setVisibility(View.VISIBLE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSpendImage.setVisibility(View.VISIBLE);
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointSpend.setText(String.format(
                    AppCache.getInstance().getMContext().getResources().getString(R.string.charge_price_simple),
                    chargeInfo.getCurrentElePrice()));
        }

        if (poiInfoEntity.getMChargeChildType() == 311) {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointTag.setText(AppCache.getInstance()
                    .getMContext().getResources().getString(R.string.route_along_service));
        } else {
            holder.mRouteReplaceSupplementItemBinding.tvReplacePointTag.setText(AppCache.getInstance()
                    .getMContext().getResources().getString(R.string.route_off_highway));
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteReplaceSupplementItemBinding mRouteReplaceSupplementItemBinding;

        public Holder(@NonNull final RouteReplaceSupplementItemBinding routeReplaceSupplementItemBinding) {
            super(routeReplaceSupplementItemBinding.getRoot());
            this.mRouteReplaceSupplementItemBinding = routeReplaceSupplementItemBinding;
            mRouteReplaceSupplementItemBinding.setHolder(this);
        }
    }

    public ArrayList<PoiInfoEntity> getPoiInfoEntities() {
        return mPoiInfoEntities;
    }

    public ArrayList<RouteAlterChargeStationInfo> getRouteAlterChargeStationInfo() {
        return mRouteAlterChargeStationInfo;
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param poiInfoEntity poi详情数据
         */
        void onItemClick(PoiInfoEntity poiInfoEntity);

    }
}
