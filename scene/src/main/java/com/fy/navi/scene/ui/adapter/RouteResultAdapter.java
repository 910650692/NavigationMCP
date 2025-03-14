package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteLineInfoResultItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class RouteResultAdapter extends RecyclerView.Adapter<RouteResultAdapter.Holder> {
    private List<RouteLineInfo> mRouteBeanList;
    OnItemClickListener itemClickListener;
    private int currentIndex = -1;

    public RouteResultAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    public void setRouteBeanList(List<RouteLineInfo> routeBeanList) {
        if (null == routeBeanList) {
            return;
        }

        if (routeBeanList.isEmpty()) {
            currentIndex = -1;
        } else {
            currentIndex = 0;
        }

        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        RouteLineInfoResultItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_line_info_result_item, parent, false);
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
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.routeLineInfoResultItemBinding.setModel(mRouteBeanList.get(position));
        int index = position;
        setSelectStatus(holder, currentIndex == position ? true : false);
        holder.routeLineInfoResultItemBinding.routeItemNum.setText("" + (position + NumberUtils.NUM_1));
        holder.routeLineInfoResultItemBinding.itemRootView.setOnClickListener(v -> {
            boolean isSelectIndex = currentIndex == index;
            if (itemClickListener != null) {
                itemClickListener.onItemClick(position, isSelectIndex);
            }
        });
    }

    public void setSelectIndex(int index) {
        currentIndex = index;
        notifyDataSetChanged();
    }

    public RouteLineInfo getSelectLineInfo() {
        return mRouteBeanList.get(currentIndex);
    }

    public int getCurrentIndex() {
        return currentIndex;
    }

    private void setSelectStatus(Holder holder, boolean select) {
        if (select) {
            holder.routeLineInfoResultItemBinding.itemRootView.setBackgroundColor(AppContext.mContext.getResources().getColor(R.color.bg_route_item_select));
            holder.routeLineInfoResultItemBinding.routeItemTag.setTextColor(AppContext.mContext.getResources().getColor(R.color.bg_route_big_window_select));
            holder.routeLineInfoResultItemBinding.routeItemDistance.setTextColor(AppContext.mContext.getResources().getColor(R.color.black));
            holder.routeLineInfoResultItemBinding.routeItemPrice.setTextColor(AppContext.mContext.getResources().getColor(R.color.black));
            holder.routeLineInfoResultItemBinding.routeItemTrafficLightValue.setTextColor(AppContext.mContext.getResources().getColor(R.color.black));
            holder.routeLineInfoResultItemBinding.routeItemElectricity.setTextColor(AppContext.mContext.getResources().getColor(R.color.black));
            holder.routeLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money);
            holder.routeLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt);
            holder.routeLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity);

        } else {
            holder.routeLineInfoResultItemBinding.itemRootView.setBackgroundColor(AppContext.mContext.getResources().getColor(R.color.bg_route_item_unselect));
            holder.routeLineInfoResultItemBinding.routeItemTag.setTextColor(AppContext.mContext.getResources().getColor(R.color.text_route_defult));
            holder.routeLineInfoResultItemBinding.routeItemDistance.setTextColor(AppContext.mContext.getResources().getColor(R.color.text_color_route_item_no_select));
            holder.routeLineInfoResultItemBinding.routeItemPrice.setTextColor(AppContext.mContext.getResources().getColor(R.color.text_color_route_item_no_select));
            holder.routeLineInfoResultItemBinding.routeItemTrafficLightValue.setTextColor(AppContext.mContext.getResources().getColor(R.color.text_color_route_item_no_select));
            holder.routeLineInfoResultItemBinding.routeItemElectricity.setTextColor(AppContext.mContext.getResources().getColor(R.color.text_color_route_item_no_select));
            holder.routeLineInfoResultItemBinding.routeItemTrafficLight.setImageResource(R.drawable.img_route_money_unselect);
            holder.routeLineInfoResultItemBinding.routeItemTrafficPrice.setImageResource(R.drawable.img_route_lingt_unselect);
            holder.routeLineInfoResultItemBinding.routeItemElectricityImg.setImageResource(R.drawable.img_route_electricity_unselect);
        }

    }

    public class Holder extends RecyclerView.ViewHolder {
        public RouteLineInfoResultItemBinding routeLineInfoResultItemBinding;

        public Holder(RouteLineInfoResultItemBinding routeLineInfoResultItemBinding) {
            super(routeLineInfoResultItemBinding.getRoot());
            this.routeLineInfoResultItemBinding = routeLineInfoResultItemBinding;
            routeLineInfoResultItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int index, boolean isSelectIndex);
    }
}