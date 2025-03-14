package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteLineViaPoiItemBinding;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RouteViaPointAdapter extends RecyclerView.Adapter<RouteViaPointAdapter.Holder> {
    private List<RouteParam> mRouteParams;
    private OnDeleteViaPointClickListener itemClickListener;

    public RouteViaPointAdapter() {
        mRouteParams = new ArrayList<>();
    }

    public void setRouteBeanList(List<RouteParam> routeParams) {
        if (ConvertUtils.isEmpty(routeParams)) {
            return;
        }
        mRouteParams.clear();
        mRouteParams.addAll(routeParams);
        notifyDataSetChanged();
    }

    public void setDeleteViaPointListener(OnDeleteViaPointClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        RouteLineViaPoiItemBinding routeItemBinding =
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
    public void onBindViewHolder(@NonNull Holder holder, @SuppressLint("RecyclerView") int position) {
        if (position == mRouteParams.size() - 1) {
            holder.routeLineViaPoiItemBinding.routeItemViaEmpty.setVisibility(View.GONE);
            holder.routeLineViaPoiItemBinding.routeItemViaEmptyLine.setVisibility(View.GONE);
        } else {
            holder.routeLineViaPoiItemBinding.routeItemViaEmpty.setVisibility(View.VISIBLE);
            holder.routeLineViaPoiItemBinding.routeItemViaEmptyLine.setVisibility(View.VISIBLE);
        }
        holder.routeLineViaPoiItemBinding.setModel(mRouteParams.get(position));
        holder.routeLineViaPoiItemBinding.routeItemViaDeleteImg.setOnClickListener(view -> {
            if (ConvertUtils.isEmpty(itemClickListener)) return;
            itemClickListener.onDeleteViaPointClick(position);
            mRouteParams.remove(position);
            notifyDataSetChanged();
        });
    }

    public void onItemMove(int adapterPosition, int targetPosition) {
        if (ConvertUtils.isEmpty(mRouteParams)) return;
        if (mRouteParams.size() <= adapterPosition || mRouteParams.size() <= targetPosition) return;
        Collections.swap(mRouteParams, adapterPosition, targetPosition);
        notifyItemMoved(adapterPosition, targetPosition);
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RouteLineViaPoiItemBinding routeLineViaPoiItemBinding;

        public Holder(RouteLineViaPoiItemBinding routeLineViaPoiItemBinding) {
            super(routeLineViaPoiItemBinding.getRoot());
            this.routeLineViaPoiItemBinding = routeLineViaPoiItemBinding;
            routeLineViaPoiItemBinding.setHolder(this);
        }
    }

    public interface OnDeleteViaPointClickListener {
        void onDeleteViaPointClick(int index);
    }
}