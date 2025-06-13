package com.fy.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSupplementItemBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteSupplementInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;


public class RouteSupplementAdapter extends RecyclerView.Adapter<RouteSupplementAdapter.Holder>{
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;

    public RouteSupplementAdapter() {
        mRouteSupplementInfos = new ArrayList<>();
    }

    /***
     * 设置补能点显示
     * @param routeSupplementInfos 子节点列表
     */
    public void setRouteSupplementInfos(final ArrayList<RouteSupplementInfo> routeSupplementInfos) {
        if (null == routeSupplementInfos) {
            return;
        }
        mRouteSupplementInfos.clear();
        mRouteSupplementInfos.addAll(routeSupplementInfos);
        notifyDataSetChanged();
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
        } else if (position == getItemCount() -1){
            holder.mRouteSupplementItemBinding.viewSupplementEnd.setVisibility(View.GONE);
        }
        final RouteSupplementInfo routeSupplementInfo = mRouteSupplementInfos.get(position);
        if (routeSupplementInfo.getMType() == AutoMapConstant.SupplementType.SUPPLEMENT_POINT) {
            final RouteChargeStationDetailInfo routeChargeStationDetailInfo = routeSupplementInfo.getMRouteChargeStationDetailInfo();
            holder.mRouteSupplementItemBinding.tvSupplementName.setText(routeChargeStationDetailInfo.getMName());
            holder.mRouteSupplementItemBinding.tvSupplementDistance.setText(routeSupplementInfo.getMUnitDistance());
        }

    }

    @Override
    public int getItemCount() {
        return mRouteSupplementInfos.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteSupplementItemBinding mRouteSupplementItemBinding;

        public Holder(@NonNull final RouteSupplementItemBinding routeSupplementItemBinding) {
            super(routeSupplementItemBinding.getRoot());
            this.mRouteSupplementItemBinding = routeSupplementItemBinding;
            mRouteSupplementItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param childInfo 子节点对象
         * @param poiInfoEntity 当前位置
         */
        void onItemClick(ChildInfo childInfo, PoiInfoEntity poiInfoEntity);

    }
}
