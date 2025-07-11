package com.sgm.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteSupplementItemBinding;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.route.RouteChargeStationDetailInfo;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;


public class RouteSupplementAdapter extends RecyclerView.Adapter<RouteSupplementAdapter.Holder>{
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;
    private ArrayList<PoiInfoEntity> mPoiInfoEntities;
    private ConcurrentHashMap<Integer, RouteAlterChargeStationParam> mAlterChargeStation;
    private ConcurrentHashMap<Integer, RouteAlterChargeStationParam> mAlterChargeStationShow;
    private OnItemClickListener mItemClickListener;

    public RouteSupplementAdapter() {
        mRouteSupplementInfos = new ArrayList<>();
        mPoiInfoEntities = new ArrayList<>();
        mAlterChargeStation = new ConcurrentHashMap<>();
        mAlterChargeStationShow = new ConcurrentHashMap<>();
    }

    /***
     * 设置补能点显示
     * @param routeSupplementInfos 子节点列表
     * @param poiInfoEntities 详情数据
     */
    public void setRouteSupplementInfos(final ArrayList<RouteSupplementInfo> routeSupplementInfos,
                                        final ArrayList<PoiInfoEntity> poiInfoEntities) {
        ThreadManager.getInstance().postUi(() -> {
            if (null == routeSupplementInfos || poiInfoEntities == null) {
                return;
            }
            mRouteSupplementInfos.clear();
            mPoiInfoEntities.clear();
            mRouteSupplementInfos.addAll(routeSupplementInfos);
            mPoiInfoEntities.addAll(poiInfoEntities);
            notifyDataSetChanged();
        });
    }


    /***
     * 设置可替换补能点数据
     * @param routeAlterChargeStationParam 替换充电站搜索信息
     * @param index index
     */
    public void setAlterChargeStation(final RouteAlterChargeStationParam routeAlterChargeStationParam, final int index) {
        ThreadManager.getInstance().postUi(() -> {
            mAlterChargeStation.put(index, routeAlterChargeStationParam);
            if (mPoiInfoEntities.size() > index) {
                notifyItemChanged(index);
            }
        });
    }

    /***
     * 清除可替换补能点数据
     */
    public void clearAlterChargeStation() {
        ThreadManager.getInstance().postUi(() -> {
            mAlterChargeStation.clear();
        });
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteSupplementItemBinding routeSupplementItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_supplement_item, parent, false);
        return new RouteSupplementAdapter.Holder(routeSupplementItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mRouteSupplementItemBinding.tvSupplementIndex.setText(String.valueOf(position + 1));
        holder.mRouteSupplementItemBinding.viewSupplementStart.setVisibility(View.VISIBLE);
        holder.mRouteSupplementItemBinding.viewSupplementEnd.setVisibility(View.VISIBLE);
        if (position == 0) {
            holder.mRouteSupplementItemBinding.viewSupplementStart.setVisibility(View.GONE);
        }
        if (position == getItemCount() -1){
            holder.mRouteSupplementItemBinding.viewSupplementEnd.setVisibility(View.GONE);
        }
        holder.mRouteSupplementItemBinding.tvSupplementIndex.setText(String.valueOf(position + 1));

        if (position < mPoiInfoEntities.size()) {
            final PoiInfoEntity poiInfoEntity = mPoiInfoEntities.get(position);
            setPoiInfoEntityView(holder, poiInfoEntity);
        }

        RouteAlterChargeStationParam routeAlterChargeStationParam = mAlterChargeStation.get(position);
        setAlterChargeStationView(holder, routeAlterChargeStationParam, position);

        if (position < mRouteSupplementInfos.size()) {
            final RouteSupplementInfo routeSupplementInfo = mRouteSupplementInfos.get(position);
            setRouteSupplementInfoView(holder, routeSupplementInfo);
        }

    }

    @Override
    public int getItemCount() {
        return mPoiInfoEntities.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSupplementItemBinding mRouteSupplementItemBinding;

        public Holder(@NonNull final RouteSupplementItemBinding routeSupplementItemBinding) {
            super(routeSupplementItemBinding.getRoot());
            this.mRouteSupplementItemBinding = routeSupplementItemBinding;
            mRouteSupplementItemBinding.setHolder(this);
        }
    }

    /**
     * 设置详情数据
     *
     * @param holder holder
     * @param poiInfoEntity POI详情参数
     */
    public void setPoiInfoEntityView(@NonNull final Holder holder, final PoiInfoEntity poiInfoEntity) {
        holder.mRouteSupplementItemBinding.tvSupplementName.setText(poiInfoEntity.getMName());
        holder.mRouteSupplementItemBinding.tvSupplementAddress.setText(poiInfoEntity.getAddress());
        final ChargeInfo chargeInfo = poiInfoEntity.getChargeInfoList().get(0);
        final String fastFree = chargeInfo.getFast_free() == -1 ? "" : String.valueOf(chargeInfo.getFast_free());
        final String fastTotal = chargeInfo.getFast_total() == -1 ? "" : "/" + chargeInfo.getFast_total();
        holder.mRouteSupplementItemBinding.tvSupplementFastCurrent.setText(fastFree);
        holder.mRouteSupplementItemBinding.tvSupplementFastTotal.setText(fastTotal);
        holder.mRouteSupplementItemBinding.tvSupplementFastTotal.setVisibility(View.VISIBLE);
        holder.mRouteSupplementItemBinding.tvSupplementFastCurrent.setVisibility(View.VISIBLE);
        holder.mRouteSupplementItemBinding.tvSupplementFastTitle.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(fastFree) && ConvertUtils.isEmpty(fastTotal)) {
            holder.mRouteSupplementItemBinding.tvSupplementFastTotal.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementFastCurrent.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementFastTitle.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastFree)) {
            holder.mRouteSupplementItemBinding.tvSupplementFastCurrent.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementFastTotal.setText(fastTotal.substring(1));
        }

        final String slowFree = chargeInfo.getSlow_free() == -1 ? "" : String.valueOf(chargeInfo.getSlow_free());
        final String slowTotal = chargeInfo.getSlow_total() == -1 ? "" : "/" + chargeInfo.getSlow_total();
        holder.mRouteSupplementItemBinding.tvSupplementSlowCurrent.setText(slowFree);
        holder.mRouteSupplementItemBinding.tvSupplementSlowTotal.setText(slowTotal);
        holder.mRouteSupplementItemBinding.tvSupplementSlowTotal.setVisibility(View.VISIBLE);
        holder.mRouteSupplementItemBinding.tvSupplementSlowCurrent.setVisibility(View.VISIBLE);
        holder.mRouteSupplementItemBinding.tvSupplementSlowTitle.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(slowFree) && ConvertUtils.isEmpty(slowTotal)) {
            holder.mRouteSupplementItemBinding.tvSupplementSlowTotal.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementSlowCurrent.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementSlowTitle.setVisibility(View.GONE);
        } else if (ConvertUtils.isEmpty(fastFree)) {
            holder.mRouteSupplementItemBinding.tvSupplementSlowCurrent.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementSlowTotal.setText(slowTotal.substring(1));
        }

        if (chargeInfo.getCurrentElePrice().startsWith("-")) {
            holder.mRouteSupplementItemBinding.tvSupplementSpendImage.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementSpend.setVisibility(View.GONE);
        } else {
            holder.mRouteSupplementItemBinding.tvSupplementSpendImage.setVisibility(View.VISIBLE);
            holder.mRouteSupplementItemBinding.tvSupplementSpend.setVisibility(View.VISIBLE);
            holder.mRouteSupplementItemBinding.tvSupplementSpend.setText(String.format(
                    AppCache.getInstance().getMContext().getResources().getString(R.string.charge_price_simple),
                    chargeInfo.getCurrentElePrice()));
        }

        if (poiInfoEntity.getMChargeChildType() == 311) {
            holder.mRouteSupplementItemBinding.tvSupplementTag.setText(AppCache.getInstance()
                    .getMContext().getResources().getString(R.string.route_along_service));
        } else {
            holder.mRouteSupplementItemBinding.tvSupplementTag.setText(AppCache.getInstance()
                    .getMContext().getResources().getString(R.string.route_off_highway));
        }
    }

    /**
     * 设置补能点数据数据
     *
     * @param holder holder
     * @param routeSupplementInfo 补能点参数
     */
    public void setRouteSupplementInfoView(@NonNull final Holder holder, final RouteSupplementInfo routeSupplementInfo) {
        holder.mRouteSupplementItemBinding.tvSupplementDistance.setText(routeSupplementInfo.getMUnitDistance());
        if (routeSupplementInfo.getMType() == AutoMapConstant.SupplementType.SUPPLEMENT_POINT) {
            RouteChargeStationDetailInfo routeChargeStationDetailInfo = routeSupplementInfo.getMRouteChargeStationDetailInfo();
            holder.mRouteSupplementItemBinding.tvSupplementInfo.setText(String.format(
                    AppCache.getInstance().getMContext().getResources().getString(R.string.route_supplement_achieve),
                    String.valueOf((int )Math.floor(routeChargeStationDetailInfo.getMRemainingPercent() * 100)),
                    String.valueOf(routeChargeStationDetailInfo.getMChargeTime()/60),
                    String.valueOf(routeChargeStationDetailInfo.getMChargePercent())));
        } else {
            holder.mRouteSupplementItemBinding.tvSupplementInfo.setText(
                    AppCache.getInstance().getMContext().getResources().getString(R.string.route_replace_supplement_achieve));
            holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setText(
                    AppCache.getInstance().getMContext().getResources().getString(R.string.route_no_replace_supplement));
            holder.mRouteSupplementItemBinding.ivSupplementReplace.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setVisibility(View.VISIBLE);
        }
    }

    /**
     * 设置替换补能点数据数据
     *
     * @param holder holder
     * @param routeAlterChargeStationParam 替换补能点参数
     * @param position index
     */
    public void setAlterChargeStationView(@NonNull final Holder holder,
                                          final RouteAlterChargeStationParam routeAlterChargeStationParam,
                                          final int position) {
        if (routeAlterChargeStationParam != null) {
            ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos = routeAlterChargeStationParam.getMRouteAlterChargeStationInfos();
            if (routeAlterChargeStationInfos == null || routeAlterChargeStationInfos.isEmpty()) {
                holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setText(
                        AppCache.getInstance().getMContext().getResources().getString(R.string.route_no_replace_supplement));
                holder.mRouteSupplementItemBinding.ivSupplementReplace.setVisibility(View.GONE);
                holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.GONE);
                holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setVisibility(View.VISIBLE);
            } else {
                if (position < mRouteSupplementInfos.size()) {
                    final RouteSupplementInfo routeSupplementInfo = mRouteSupplementInfos.get(position);
                    for (RouteAlterChargeStationInfo routeAlterChargeStationInfo : routeAlterChargeStationInfos) {
                        routeAlterChargeStationInfo.setMDistance(routeAlterChargeStationInfo.getMDetourInfo().getDistance()
                                + routeSupplementInfo.getMDistance());
                    }
                }

                holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setText(String.format(
                        AppCache.getInstance().getMContext().getResources().getString(R.string.route_has_replace_supplement)
                        , routeAlterChargeStationInfos.size()));
                holder.mRouteSupplementItemBinding.ivSupplementReplace.setBackground(ResourceUtils
                        .Companion.getInstance().getDrawable(R.drawable.img_route_down));
                holder.mRouteSupplementItemBinding.ivSupplementReplace.setVisibility(View.VISIBLE);
                holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setVisibility(View.VISIBLE);
                holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.GONE);
                RouteReplaceSupplementAdapter routeReplaceSupplementAdapter = new RouteReplaceSupplementAdapter();
                routeReplaceSupplementAdapter.setRouteSupplementInfos(routeAlterChargeStationInfos);
                holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setLayoutManager(new LinearLayoutManager(AppCache.getInstance().getMContext()));
                holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setAdapter(routeReplaceSupplementAdapter);
                holder.mRouteSupplementItemBinding.ivSupplementReplace.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        if (holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.getVisibility() == View.VISIBLE) {
                            holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.GONE);
                            holder.mRouteSupplementItemBinding.ivSupplementReplace.setBackground(ResourceUtils
                                    .Companion.getInstance().getDrawable(R.drawable.img_route_down));
                            mAlterChargeStationShow.remove(position);
                        } else {
                            holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.VISIBLE);
                            holder.mRouteSupplementItemBinding.ivSupplementReplace.setBackground(ResourceUtils
                                    .Companion.getInstance().getDrawable(R.drawable.img_route_up));
                            ArrayList<PoiInfoEntity> replaceSupplementPoi =  routeReplaceSupplementAdapter.getPoiInfoEntities();
                            if ((replaceSupplementPoi == null || replaceSupplementPoi.isEmpty()) && mItemClickListener != null) {
                                mItemClickListener.onExpandClick(routeReplaceSupplementAdapter, routeReplaceSupplementAdapter.getRouteAlterChargeStationInfo());
                            }
                            RouteAlterChargeStationParam alterCharge = mAlterChargeStation.get(position);
                            if (mAlterChargeStationShow.get(position) == null && alterCharge != null) {
                                mAlterChargeStationShow.put(position, alterCharge);
                            }
                        }
                        if (mItemClickListener != null) {
                            if (mAlterChargeStationShow == null || mAlterChargeStationShow.isEmpty()) {
                                mItemClickListener.onAlterShowClick(null);
                            } else {
                                ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos = new ArrayList<>();
                                for (RouteAlterChargeStationParam alterChargeStation : mAlterChargeStationShow.values()){
                                    if (alterChargeStation.getMRouteAlterChargeStationInfos() == null) {
                                        continue;
                                    }
                                    routeAlterChargeStationInfos.addAll(alterChargeStation.getMRouteAlterChargeStationInfos());
                                }
                                mItemClickListener.onAlterShowClick(routeAlterChargeStationInfos);
                            }
                        }
                    }
                });

                routeReplaceSupplementAdapter.setItemClickListener(new RouteReplaceSupplementAdapter.OnItemClickListener() {
                    @Override
                    public void onItemClick(PoiInfoEntity newPoiInfoEntity) {
                        if (mItemClickListener != null && position < mPoiInfoEntities.size()) {
                            mItemClickListener.onItemClick(newPoiInfoEntity, mPoiInfoEntities.get(position));
                        }
                    }

                    @Override
                    public void onItemDetailsClick(PoiInfoEntity poiInfoEntity) {
                        if (mItemClickListener != null ) {
                            mItemClickListener.onItemDetailsClick(poiInfoEntity, mPoiInfoEntities.get(position));
                        }
                    }
                });
            }
        } else {
            holder.mRouteSupplementItemBinding.tvSupplementReplaceTitle.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.ivSupplementReplace.setVisibility(View.GONE);
            holder.mRouteSupplementItemBinding.tvSupplementReplaceItem.setVisibility(View.GONE);
        }
    }

    public void setItemClickListener(OnItemClickListener mItemClickListener) {
        this.mItemClickListener = mItemClickListener;
    }

    public interface OnItemClickListener {

        /***
         * 替换补能点展开点击事件
         * @param routeReplaceSupplementAdapter 子界面adapter
         * @param routeAlterChargeStationInfos 补能点数据
         */
        void onExpandClick(RouteReplaceSupplementAdapter routeReplaceSupplementAdapter,
                           ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos);

        /***
         * 子节点替换点击事件
         * @param newPoiInfoEntity 替换充电站
         * @param oldPoiInfoEntity 被替换充电站
         */
        void onItemClick(PoiInfoEntity newPoiInfoEntity, PoiInfoEntity oldPoiInfoEntity);

        /***
         * 备选补能点扎点事件
         * @param routeAlterChargeStationInfos 扎点替换充电站
         */
        void onAlterShowClick(ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos);

        /***
         * 子节点详情点击事件
         * @param newPoiInfoEntity 替换充电站
         * @param oldPoiInfoEntity 被替换充电站
         */
        void onItemDetailsClick(PoiInfoEntity newPoiInfoEntity, PoiInfoEntity oldPoiInfoEntity);

    }
}
