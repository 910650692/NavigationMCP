package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.scene.ui.setting.DownloadBtnView;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.view.CustomSwipeMenuLayout;
import com.fy.navi.ui.view.SkinButton;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;

public class MapDataAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private static final int TYPE_PARENT = 0;
    private static final int TYPE_CHILD = 1;
    private Context mContext;
    private List<ProvDataInfo> parentItems = new ArrayList<>();
    private OnChildClickListener onChildClickListener;
    private boolean isClickAllStart = true;
    private boolean isClickAllPause = false;

    public MapDataAdapter(final Context context) {
        this.mContext = context;
    }

    public void setData(List<ProvDataInfo> parentItems) {
        this.parentItems = parentItems;
        notifyDataSetChanged();
    }

    public void setOnChildClickListener(final OnChildClickListener listener) {
        this.onChildClickListener = listener;
    }

    @Override
    public int getItemViewType(int position) {
        int parentIndex = 0;
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                return TYPE_PARENT;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    return TYPE_CHILD;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        if (viewType == TYPE_PARENT) {
            View view = inflater.inflate(R.layout.item_province, parent, false);
            return new ParentViewHolder(view);
        } else {
            View view = inflater.inflate(R.layout.item_city_data, parent, false);
            return new ChildViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        int parentIndex = 0;
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                ((ParentViewHolder) holder).bind(parent);
                return;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    int childPos = position - itemCount;
                    ((ChildViewHolder) holder).bind(parent, parent.getCityInfoList().get(childPos));
                    return;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public int getItemCount() {
        if (parentItems != null && !parentItems.isEmpty()) {
            int count = parentItems.size();
            for (ProvDataInfo parent : parentItems) {
                if (parent.isExpanded()) {
                    count += parent.getCityInfoList().size();
                }
            }
            return count;
        } else {
            return 0;
        }
    }

    /**
     * 父项ViewHolder
     */
    class ParentViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView title;
        private SkinImageView expandIcon;

        ParentViewHolder(View itemView) {
            super(itemView);
            title = itemView.findViewById(R.id.item_province_name);
            expandIcon = itemView.findViewById(R.id.item_province_expand);

            itemView.setOnClickListener(v -> {
                // 使用绝对位置方法（兼容新旧版本）
                int position = getAbsoluteAdapterPosition();
                if (position != RecyclerView.NO_POSITION) {
                    // 获取对应的父项数据
                    ProvDataInfo parent = parentItems.get(getParentPosition(position));
                    // 记录之前的展开状态
                    boolean wasExpanded = parent.isExpanded();
                    // 切换展开状态
                    parent.setExpanded(!wasExpanded);
                    // 局部刷新当前父项（只更新这一项）
                    notifyItemChanged(position);
                    // 计算子项数量
                    int childCount = parent.getCityInfoList().size();
                    if (wasExpanded) {
                        // 如果之前是展开状态，现在要折叠 - 移除子项
                        notifyItemRangeRemoved(position + 1, childCount);
                    } else {
                        // 如果之前是折叠状态，现在要展开 - 添加子项
                        notifyItemRangeInserted(position + 1, childCount);
                    }
                }
            });
        }

        void bind(ProvDataInfo parent) {
            title.setText(parent.getName()); //省份名称
            expandIcon.setImageResource(
                    parent.isExpanded() ? R.drawable.img_up_58 : R.drawable.img_under_the_58
            );
        }
    }

    /**
     * 子项ViewHolder
     */
    class ChildViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView cityName;
        private SkinTextView cityData;
        private DownloadBtnView mDownloadBtnView;
        private CustomSwipeMenuLayout swipeMenuLayout;
        private SkinButton allDownload;
        private SkinButton allPause;
        private SkinTextView deleteCity;
        private ProgressBar downloadProgress;

        ChildViewHolder(View itemView) {
            super(itemView);
            cityName = itemView.findViewById(R.id.item_city_name);
            cityData = itemView.findViewById(R.id.item_city_data);
            swipeMenuLayout = itemView.findViewById(R.id.swipe_menu_layout);
            mDownloadBtnView = itemView.findViewById(R.id.item_download_status);
            allDownload = itemView.findViewById(R.id.item_all_download);
            allPause = itemView.findViewById(R.id.item_all_pause);
            deleteCity = itemView.findViewById(R.id.item_driving_delete);
            downloadProgress = itemView.findViewById(R.id.download_progress);
        }

        void bind(ProvDataInfo parent, CityDataInfo child) {

            final CityDownLoadInfo downloadItem = child.getDownLoadInfo();
            if (downloadItem == null) {
                return;
            }
            // 非已下载状态，禁止侧滑删除
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                swipeMenuLayout.setSwipeEnabled(true);
            } else {
                swipeMenuLayout.setSwipeEnabled(false);
                swipeMenuLayout.smoothClose();
            }
            // 城市名称
            cityName.setText(child.getName());
            // 城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize().longValue());
            cityData.setText(sizeString);

            if (child.getName().equals("全省下载")) {
                mDownloadBtnView.setVisibility(View.GONE);
                allDownload.setVisibility(View.VISIBLE);
                allPause.setVisibility(View.VISIBLE);
                List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                for (CityDataInfo info : cityDataInfos) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                        cityAdCodes.add(info.getAdcode());
                    }
                }
                if (cityDataInfos.size() == cityAdCodes.size()) { // 该省份下所有城市均已下载
                    isClickAllStart = false;
                }
                setAllStartAndPauseStatus(allDownload, allPause, isClickAllStart, isClickAllPause);
            } else {
                mDownloadBtnView.setVisibility(View.VISIBLE);
                allDownload.setVisibility(View.GONE);
                allPause.setVisibility(View.GONE);
                //下载按钮状态
                mDownloadBtnView.parseDownloadStatusInfo(child.getDownLoadInfo());
                final boolean isShowDownloadProgress = child.getDownLoadInfo().getTaskState()  == UserDataCode.TASK_STATUS_CODE_DOING
                        || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE
                        || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_UNZIPPING
                        || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE;
                if (isShowDownloadProgress) {
                    downloadProgress.setProgress((int) Math.floor(child.getDownLoadInfo().getPercent()));
                    downloadProgress.setVisibility(View.VISIBLE);
                } else {
                    downloadProgress.setVisibility(View.GONE);
                }
            }

            // 下载 or 暂停下载
            mDownloadBtnView.setOnClickListener(v -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null) {
                    switch (downloadItem.getTaskState()) {
                        case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                            onChildClickListener.pauseAllTask(cityAdCodes);
                            break;
                        case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                        case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                        case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                        case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                            onChildClickListener.startAllTask(cityAdCodes);
                            break;
                        default:
                            break;
                    }
                }
            });
            // 删除已下载数据
            deleteCity.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null) {
                    onChildClickListener.deleteAllTask(cityAdCodes);
                }
            });
            // 全部开始下载
            allDownload.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                setAllStartAndPauseStatus(allDownload, allPause,false, true);
                List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                for (CityDataInfo info : cityDataInfos) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX  ) {
                        cityAdCodes.add(info.getAdcode());
                    }
                }
                if (onChildClickListener != null) {
                    onChildClickListener.allDownloadTask(cityAdCodes);
                }
            });
            // 全部暂停下载
            allPause.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                setAllStartAndPauseStatus(allDownload, allPause,false, true);
                List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                for (CityDataInfo info : cityDataInfos) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                        cityAdCodes.add(info.getAdcode());
                    }
                }
                if (onChildClickListener != null) {
                    onChildClickListener.allPauseTask(cityAdCodes);
                }
            });
        }
    }

    /**
     * 设置全部开始/全部暂停按钮状态
     * @param isStart 全部开始按钮是否可点击，默认可点击
     * @param isPause 全部暂停按钮是否可点击，默认不可点击
     */
    private void setAllStartAndPauseStatus(SkinButton allDownload, SkinButton allPause, final boolean isStart, final boolean isPause) {
        isClickAllStart = isStart;
        isClickAllPause = isPause;
        if (isClickAllStart) {
            allDownload.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
            allDownload.setBackgroundResource(R.drawable.shape_bg_download_data);
            allDownload.setEnabled(true);
            allDownload.setAlpha(1.0f);
        } else {
            allDownload.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_70_000000));
            allDownload.setBackgroundResource(R.drawable.shape_bg_map_item_data);
            allDownload.setEnabled(false);
            allDownload.setAlpha(0.7f);
        }

        if (isClickAllPause) {
            allPause.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
            allPause.setBackgroundResource(R.drawable.shape_bg_download_data);
            allPause.setEnabled(true);
            allPause.setAlpha(1.0f);
        } else {
            allPause.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_70_000000));
            allPause.setBackgroundResource(R.drawable.shape_bg_map_item_data);
            allPause.setEnabled(false);
            allPause.setAlpha(0.7f);
        }
    }

    /**
     * 更新子项数据
     * @param parentId
     * @param childId
     * @param newValue
     */
    public void updateChild(int parentId, int childId, CityDownLoadInfo newValue) {
        int childPosition = getChildPosition(parentId, childId);
        for (ProvDataInfo parent : parentItems) {
            if (parent.getAdcode() == parentId) {
                for (CityDataInfo child : parent.getCityInfoList()) {
                    if (child.getAdcode() == childId) {
                        child.setDownLoadInfo(newValue);
                        notifyItemChanged(childPosition);
                        return;
                    }
                }
            }
        }
    }

    /**
     * 获取父项在原始列表中的位置（不考虑子项）
     * @param adapterPosition
     * @return
     */
    private int getParentPosition(int adapterPosition) {
        int parentPos = 0;
        int currentPos = 0;

        for (ProvDataInfo parent : parentItems) {
            if (adapterPosition == currentPos) {
                return parentPos;
            }
            currentPos++;

            if (parent.isExpanded()) {
                if (adapterPosition <= currentPos + parent.getCityInfoList().size() - 1) {
                    return parentPos;
                }
                currentPos += parent.getCityInfoList().size();
            }

            parentPos++;
        }
        return -1;
    }



    /**
     * 新增方法：根据父项和子项ID获取子项的绝对位置
     * @param parentId
     * @param childId
     * @return
     */
    public int getChildPosition(int parentId, int childId) {
        int position = 0;

        for (ProvDataInfo parent : parentItems) {
            position++; // 父项位置

            if (parent.getAdcode() == parentId) {
                if (parent.isExpanded()) {
                    for (CityDataInfo child : parent.getCityInfoList()) {
                        if (child.getAdcode() == childId) {
                            return position;
                        }
                        position++;
                    }
                }
                break;
            }

            if (parent.isExpanded()) {
                position += parent.getCityInfoList().size();
            }
        }
        return -1; // 未找到
    }

    public interface OnChildClickListener {

        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 取消下载
         */
        void cancelAllTask();

        /**
         * 全部下载
         * @param cityAdCodes
         */
        void allDownloadTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 全部暂停
         * @param cityAdCodes
         */
        void allPauseTask(final ArrayList<Integer> cityAdCodes);
    }

}
