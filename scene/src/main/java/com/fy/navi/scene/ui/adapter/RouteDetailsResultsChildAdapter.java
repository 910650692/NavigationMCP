package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteDetailsInfoResultChildItemBinding;
import com.fy.navi.scene.impl.route.common.SceneRouteCommonStruct;
import com.fy.navi.scene.impl.route.common.SceneRouteDetailEnumRes;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;

import java.util.ArrayList;
import java.util.List;

public class RouteDetailsResultsChildAdapter extends RecyclerView.Adapter<RouteDetailsResultsChildAdapter.Holder> {
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos;

    public RouteDetailsResultsChildAdapter() {
        mRouteLineSegmentInfos = new ArrayList<>();
    }

    /***
     * 设置数据
     * @param routeLineSegmentInfos 数据列表
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setAdapterResult(List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        this.mRouteLineSegmentInfos = routeLineSegmentInfos;
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteDetailsInfoResultChildItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_details_info_result_child_item, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteLineSegmentInfos == null) {
            return 0;
        }
        return mRouteLineSegmentInfos.size();
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mRouteDetailsInfoResultChildItemBinding.routeDetailInfoItemChildImg.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(
                SceneRouteCommonStruct.RouteDetailsMainAction.get(mRouteLineSegmentInfos.get(position).getMIconType())).getDayDrawableId());
        holder.mRouteDetailsInfoResultChildItemBinding.routeDetailInfoItemChildDescription.setText(mRouteLineSegmentInfos.get(position).getMDistance() + " "
                + AppCache.getInstance().getMContext().getResources().getString(R.string.route_details_after_to_arrive)
                + mRouteLineSegmentInfos.get(position).getMLoadName());
    }

    public class Holder extends RecyclerView.ViewHolder {
        private final RouteDetailsInfoResultChildItemBinding mRouteDetailsInfoResultChildItemBinding;

        public Holder(final RouteDetailsInfoResultChildItemBinding routeDetailsInfoResultChildItemBinding) {
            super(routeDetailsInfoResultChildItemBinding.getRoot());
            this.mRouteDetailsInfoResultChildItemBinding = routeDetailsInfoResultChildItemBinding;
            routeDetailsInfoResultChildItemBinding.setHolder(this);
        }
    }
}