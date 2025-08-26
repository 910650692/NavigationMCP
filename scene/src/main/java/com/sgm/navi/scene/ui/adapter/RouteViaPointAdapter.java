package com.sgm.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteLineViaPoiItemBinding;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RouteViaPointAdapter extends RecyclerView.Adapter<RouteViaPointAdapter.Holder> {
    private List<RouteParam> mRouteParams;
    private OnDeleteViaPointClickListener mItemClickListener;

    public RouteViaPointAdapter() {
        mRouteParams = new ArrayList<>();
    }

    /***
     * 设置途径点列表信息
     * @param routeParams 途径点信息
     */
    public void setRouteBeanList(final List<RouteParam> routeParams) {
        if (ConvertUtils.isEmpty(routeParams)) {
            return;
        }
        mRouteParams.clear();
        mRouteParams.addAll(routeParams);
        notifyDataSetChanged();
    }

    public List<RouteParam> getRouteParams() {
        return mRouteParams;
    }

    public void setDeleteViaPointListener(final OnDeleteViaPointClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final RouteLineViaPoiItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_line_via_poi_item, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (ConvertUtils.isEmpty(mRouteParams)) {
            return NumberUtils.NUM_0;
        }
        return mRouteParams.size();
    }

    @Override
    public void onBindViewHolder(final @NonNull Holder holder, final @SuppressLint("RecyclerView") int position) {
        if (position == mRouteParams.size() - 1) {
            holder.mRouteLineViaPoiItemBinding.routeItemViaEmpty.setVisibility(View.GONE);
            holder.mRouteLineViaPoiItemBinding.routeItemViaEmptyLine.setVisibility(View.GONE);
        } else {
            holder.mRouteLineViaPoiItemBinding.routeItemViaEmpty.setVisibility(View.VISIBLE);
            holder.mRouteLineViaPoiItemBinding.routeItemViaEmptyLine.setVisibility(View.VISIBLE);
        }
        holder.mRouteLineViaPoiItemBinding.setModel(mRouteParams.get(position));
        holder.mRouteLineViaPoiItemBinding.routeItemViaDeleteImg.setOnClickListener(view -> {
            if (ConvertUtils.isEmpty(mItemClickListener)) {
                return;
            }
            mItemClickListener.onDeleteViaPointClick(position);
            mRouteParams.remove(position);
            notifyDataSetChanged();
        });
    }

    /***
     * 设置途径点移动
     * @param adapterPosition 途径点索引
     * @param targetPosition 目标索引
     */
    public void onItemMove(final int adapterPosition,final int targetPosition) {
        if (ConvertUtils.isEmpty(mRouteParams)) {
            return;
        }
        if (mRouteParams.size() <= adapterPosition || mRouteParams.size() <= targetPosition) {
            return;
        }
        Collections.swap(mRouteParams, adapterPosition, targetPosition);
        notifyItemMoved(adapterPosition, targetPosition);
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteLineViaPoiItemBinding mRouteLineViaPoiItemBinding;
        public View mRouteViaSelect;

        public Holder(final RouteLineViaPoiItemBinding routeLineViaPoiItemBinding) {
            super(routeLineViaPoiItemBinding.getRoot());
            this.mRouteLineViaPoiItemBinding = routeLineViaPoiItemBinding;
            routeLineViaPoiItemBinding.setHolder(this);
            mRouteViaSelect = routeLineViaPoiItemBinding.routeViaSelect;
        }
    }

    public interface OnDeleteViaPointClickListener {
        /**
         * 途径点删除点击
         * @param index 点击下标
         */
        void onDeleteViaPointClick(int index);
    }
}